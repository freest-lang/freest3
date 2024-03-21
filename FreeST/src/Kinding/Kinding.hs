{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      :  Kinding.Kinding
Description :  Check the type formation
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Kinding.Kinding
  ( synthetise
  , checkAgainst
  , checkAgainstSession
  , un
  , lin
  , checkAgainstAbsorb
  , checkContractive
  , unr
  )
where

import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error
import           Util.State
import           Kinding.Contractive
import           Kinding.Subkind ( (<:), join, meet )

import           Control.Monad.State hiding (join)
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- synthetise :: MonadState FreestS m => K.KindEnv -> T.Type -> m K.Kind
synthetise :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m K.Kind
synthetise kenv = synthetise' (Map.keysSet kenv) kenv

checkAgainst :: MonadState (FreestS a) m =>  K.KindEnv -> K.Kind -> T.Type -> m K.Kind
checkAgainst kenv = checkAgainst' (Map.keysSet kenv) kenv

checkAgainstSession :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m K.Kind
checkAgainstSession kenv = checkAgainstSession' (Map.keysSet kenv) kenv

synthetise' :: MonadState (FreestS a) m =>  K.PolyVars -> K.KindEnv -> T.Type -> m K.Kind
-- Functional types
synthetise' _ _ (T.Int    p) = return $ K.ut p
synthetise' _ _ (T.Float  p) = return $ K.ut p 
synthetise' _ _ (T.Char   p) = return $ K.ut p
synthetise' _ _ (T.String p) = return $ K.ut p
synthetise' s kEnv (T.Arrow p m t u) =
  synthetise' s kEnv t >> synthetise' s kEnv u $> K.Kind p m K.Top
synthetise' s kEnv (T.Labelled p t m) | t == T.Variant || t == T.Record = do
  ks <- tMapM (synthetise' s kEnv) m
  let K.Kind _ n _ = foldr join (K.ut defaultSpan) ks
  return $ K.Kind p n K.Top
-- Shared session types
synthetise' s kEnv (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Semi _ u@T.Message{} (T.Var _ b))))
  | a == b = checkAgainstSession' s kEnv u $> K.ua p
synthetise' _ _ (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Labelled _ T.Choice{} m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ K.ua p
-- Session types
synthetise' _ _ (T.Skip p) = return $ K.us p
synthetise' _ _ (T.End p _) = return $ K.la p
synthetise' s kEnv (T.Semi p t u) = do
  ~k1@(K.Kind _ mt vt) <- synthetise' s kEnv t
  ~k2@(K.Kind _ mu vu) <- synthetise' s kEnv u
  unless (vt <: K.Session) (addError (ExpectingSession (getSpan t) t k1))
  unless (vu <: K.Session) (addError (ExpectingSession (getSpan u) u k2))
  return $ K.Kind p (join mt mu) (meet vt vu)
synthetise' s kEnv (T.Message p _ t) =
  checkAgainst' s kEnv (K.lt p) t $> K.ls p
synthetise' s kEnv (T.Labelled _ T.Choice{} m) = do
  ks <- tMapM (checkAgainstSession' s kEnv) m
  return $ Map.foldr (\(K.Kind _ _ v1) (K.Kind p _ v2) -> K.Kind p K.Lin (meet v1 v2))
             (snd $ Map.elemAt 0 ks) ks
--  Map.foldl (flip meet) (K.ua p) <$> tMapM (checkAgainstSession' s kEnv) m
-- Session or functional
synthetise' s kEnv mu@(T.Rec _ (Bind _ a k t)) = do
--   checkContractive s a t >> checkAgainst' s (Map.insert a k kEnv) k t $> k
  checkContractive s a t
  k'@(K.Kind p m _) <- synthetise' s (Map.insert a k kEnv) t
  unless (k' <: k) (addError $ CantMatchKinds (getSpan t) k k' t) -- $> k'
--  if unr (Map.keysSet (Map.insert a k kEnv) Set.\\ s) t
  if unr s kEnv mu
    then pure $ K.Kind p m K.Absorb
    else pure k'
synthetise' s kEnv (T.Forall _ (Bind p a k t)) =
  synthetise' (Set.insert a s) (Map.insert a k kEnv) t
synthetise' _ kEnv (T.Var p a) = case kEnv Map.!? a of
  Just k -> return k
  Nothing -> addError (TypeVarNotInScope p a) $> omission p
--    return $ omission p
-- Type operators
synthetise' _ kEnv t@(T.Dualof p (T.Var _ a)) =
  case kEnv Map.!? a of
    Just k -> unless (k <: K.ls p)
            (addError (CantMatchKinds p k (K.ls p) t)) $> K.ls p
    Nothing -> addError (TypeVarNotInScope p a) $> omission p
synthetise' _ _ t@T.Dualof{} = internalError "Validation.Kinding.synthetise'" t
synthetise' _ _ t = internalError "Validation.Kinding.synthetise'.unexpectedConditions" t

-- Check the contractivity of a given type; issue an error if not
checkContractive :: MonadState (FreestS a) m => K.PolyVars -> Variable -> T.Type -> m ()
checkContractive s a t = let p = getSpan t in
  unless (contractive s a t) $ addError (TypeNotContractive p t a)

-- Check a type against a given kind

checkAgainst' :: MonadState (FreestS a) m => K.PolyVars -> K.KindEnv -> K.Kind -> T.Type -> m K.Kind
checkAgainst' s kEnv expected t = do
  actual <- synthetise' s kEnv t
  unless (actual <: expected)
    (addError (CantMatchKinds (getSpan t) expected actual t)) $> expected

-- Check whether a given type is of a session kind. In any case return the
-- kind of the type. This is a refined version of checkAgainst for a better error messages
checkAgainstSession' :: MonadState (FreestS a) m => K.PolyVars -> K.KindEnv -> T.Type -> m K.Kind
checkAgainstSession' s kEnv t = do
  k@(K.Kind _ _ p) <- synthetise' s kEnv t
  unless (p <: K.Session) (addError (ExpectingSession (getSpan t) t k)) $> k

checkAgainstAbsorb :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m K.Kind
checkAgainstAbsorb kEnv t = do
  ~k@(K.Kind _ _ p) <- synthetise kEnv t
  when (p /= K.Absorb) (addError $ UnendedSession (getSpan t) t k) $> k

-- Determine whether a given type is unrestricted
un :: MonadState (FreestS a) m => T.Type -> m Bool
un = mult K.Un

-- Determine whether a given type is linear
lin :: MonadState (FreestS a) m => T.Type -> m Bool
lin = mult K.Lin

-- Determine whether a given type is of a given multiplicity
mult :: MonadState (FreestS a) m => K.Multiplicity -> T.Type -> m Bool
mult m1 t = do
  (K.Kind _ m2 _) <- synthetise' Set.empty Map.empty t
  return $ m2 == m1

-- Unnormed lifted to types
-- unr :: Set.Set Variable -> T.Type -> Bool
-- unr s (T.Semi _ t u) = unr s t || unr s u
-- unr s (T.Rec _ (Bind _ a _ t)) = unr (Set.insert a s) t
-- unr s (T.Labelled _ (T.Choice _) m) = all (unr s) (Map.elems m)
-- unr s (T.Var _ a) = Set.member a s
-- unr _ _ = False

unr :: Set.Set Variable -> K.KindEnv -> T.Type -> Bool
unr s kEnv (T.Semi _ t u) = unr s kEnv t || unr s kEnv u
unr s kEnv (T.Rec _ (Bind _ a k t)) = unr s (Map.insert a k kEnv) t
unr s kEnv (T.Labelled _ (T.Choice _) m) = all (unr s kEnv) (Map.elems m)
unr s kEnv (T.Var _ a) = Set.notMember a s && Map.member a kEnv
unr _ _ _ = False
