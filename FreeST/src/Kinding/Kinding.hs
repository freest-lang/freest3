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
  )
where

import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error
import           Util.State
import           Kinding.Contractive
import           Kinding.Norm
import           Kinding.Subkind ( (<:), join, meet )
import           Restriction.Restriction

import           Control.Monad.State hiding (join)
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- synthetise :: MonadState FreestS m => K.KindEnv -> T.Type -> m K.Kind
synthetise :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m (K.Kind, T.Level)
synthetise kenv = synthetise' (Map.keysSet kenv) kenv

checkAgainst :: MonadState (FreestS a) m =>  K.KindEnv -> K.Kind -> T.Type -> m K.Kind
checkAgainst kenv k t = do
  when (show (getSpan k) == "9:17" ) $ debugM $ show k ++ " -> " ++ show t 
  checkAgainst' (Map.keysSet kenv) kenv k t

checkAgainstSession :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m K.Kind
checkAgainstSession kenv = checkAgainstSession' (Map.keysSet kenv) kenv

synthetise' :: MonadState (FreestS a) m =>  K.PolyVars -> K.KindEnv -> T.Type -> m (K.Kind, T.Level)
-- Functional types
synthetise' _ _ (T.Int    p) = return $ (K.ut p, T.Bottom)
synthetise' _ _ (T.Float  p) = return $ (K.ut p, T.Bottom)
synthetise' _ _ (T.Char   p) = return $ (K.ut p, T.Bottom)
synthetise' _ _ (T.String p) = return $ (K.ut p, T.Bottom)
synthetise' s kEnv (T.Arrow p m l1 l2 t u) =
  synthetise' s kEnv t >> synthetise' s kEnv u $> (K.Kind p m K.Top, l1)
synthetise' s kEnv (T.Labelled p t l m) | t == T.Variant || t == T.Record = do
  ks <- tMapM (synthetise' s kEnv) m
  let K.Kind _ n _ = foldr (join . fst) (K.ut defaultSpan) ks
  addInequality p (l, level m)
  return $ (K.Kind p n K.Top, l)
-- Shared session types
synthetise' s kEnv (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Semi _ u@T.Message{} (T.Var _ b))))
  | a == b = checkAgainstSession' s kEnv u $> (K.ua p, T.Bottom)
synthetise' _ _ (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Labelled _ T.Choice{} _ m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ (K.ua p, T.Bottom)
-- Session types
synthetise' _ _ (T.Skip p) = return $ (K.us p, level (T.Skip p))
synthetise' _ _ (T.End p _ l) = return $ (K.la p, l) 
synthetise' s kEnv (T.Semi p t u) = do
  ~k1@(K.Kind _ mt vt, _) <- synthetise' s kEnv t
  ~k2@(K.Kind _ mu vu, _) <- synthetise' s kEnv u
  unless (vt <: K.Session) (addError (ExpectingSession (getSpan t) t (fst k1)))
  unless (vu <: K.Session) (addError (ExpectingSession (getSpan u) u (fst k2)))
  addInequality p (level t, level u)
  return $ (K.Kind p (join mt mu) (meet vt vu), level t)
synthetise' s kEnv (T.Message p l _ t) = do
  addInequality p (l, level t)
  checkAgainst' s kEnv (K.lt p) t $> (K.ls p, l)
synthetise' s kEnv (T.Labelled p T.Choice{} l m) = do
  ks <- tMapM (checkAgainstSession' s kEnv) m
  addInequality p (l, level m)
  return $ (Map.foldr (\(K.Kind _ _ v1) (K.Kind p _ v2) -> K.Kind p K.Lin (meet v1 v2))
             (snd $ Map.elemAt 0 ks) ks, l)
--  Map.foldl (flip meet) (K.ua p) <$> tMapM (checkAgainstSession' s kEnv) m
-- Session or functional
synthetise' s kEnv mu@(T.Rec _ (Bind _ a k t)) = do
--   checkContractive s a t >> checkAgainst' s (Map.insert a k kEnv) k t $> k
  checkContractive s a t
  k'@(K.Kind p m _, l) <- synthetise' s (Map.insert a k kEnv) t
  unless ((fst k') <: k) (addError $ CantMatchKinds (getSpan t) k (fst k') t) -- $> k'
--  if unr (Map.keysSet (Map.insert a k kEnv) Set.\\ s) t
  if normed s mu
    then pure (fst k', T.Bottom)
    else pure $ (K.Kind p m K.Absorb, T.Bottom)
synthetise' s kEnv (T.Forall _ (Bind p a k t)) = do
  (K.Kind _ m _, _) <- synthetise' (Set.insert a s) (Map.insert a k kEnv) t
  return $ (K.Kind p m K.Top, T.Bottom)
synthetise' _ kEnv (T.Var p a) = case kEnv Map.!? a of
  Just k -> return (k, T.Bottom)
  Nothing -> addError (TypeVarNotInScope p a) $> (omission p, T.Bottom)
--    return $ omission p
-- Type operators
synthetise' _ kEnv t@(T.Dualof p (T.Var _ a)) =
  case kEnv Map.!? a of
    Just k -> unless (k <: K.ls p)
            (addError (CantMatchKinds p k (K.ls p) t)) $> (K.ls p, T.Bottom)
    Nothing -> addError (TypeVarNotInScope p a) $> (omission p, T.Bottom)
synthetise' _ _ t@T.Dualof{} = internalError "Validation.Kinding.synthetise'" t

-- Check the contractivity of a given type; issue an error if not
checkContractive :: MonadState (FreestS a) m => K.PolyVars -> Variable -> T.Type -> m ()
checkContractive s a t = let p = getSpan t in
  unless (contractive s a t) $ addError (TypeNotContractive p t a)

-- Check a type against a given kind

checkAgainst' :: MonadState (FreestS a) m => K.PolyVars -> K.KindEnv -> K.Kind -> T.Type -> m K.Kind
checkAgainst' s kEnv expected t = do
  (actual, _) <- synthetise' s kEnv t
  unless (actual <: expected)
    (addError (CantMatchKinds (getSpan t) expected actual t)) $> expected

-- Check whether a given type is of a session kind. In any case return the
-- kind of the type. This is a refined version of checkAgainst for a better error messages
checkAgainstSession' :: MonadState (FreestS a) m => K.PolyVars -> K.KindEnv -> T.Type -> m K.Kind
checkAgainstSession' s kEnv t = do
  k@(K.Kind _ _ p, l) <- synthetise' s kEnv t
  unless (p <: K.Session) (addError (ExpectingSession (getSpan t) t (fst k))) $> (fst k)

checkAgainstAbsorb :: MonadState (FreestS a) m => K.KindEnv -> T.Type -> m K.Kind
checkAgainstAbsorb kEnv t = do
  ~k@(K.Kind _ _ p, _) <- synthetise kEnv t
  when (p /= K.Absorb) (addError $ UnendedSession (getSpan t) t (fst k)) $> (fst k)

-- Determine whether a given type is unrestricted
un :: MonadState (FreestS a) m => T.Type -> m Bool
un = mult K.Un

-- Determine whether a given type is linear
lin :: MonadState (FreestS a) m => T.Type -> m Bool
lin = mult K.Lin

-- Determine whether a given type is of a given multiplicity
mult :: MonadState (FreestS a) m => K.Multiplicity -> T.Type -> m Bool
mult m1 t = do
  (K.Kind _ m2 _, _) <- synthetise' Set.empty Map.empty t
  return $ m2 == m1

-- Unnormed lifted to types
-- unr :: Set.Set Variable -> T.Type -> Bool
-- unr s (T.Semi _ t u) = unr s t || unr s u
-- unr s (T.Rec _ (Bind _ a _ t)) = unr (Set.insert a s) t
-- unr s (T.Labelled _ (T.Choice _) m) = all (unr s) (Map.elems m)
-- unr s (T.Var _ a) = Set.member a s
-- unr _ _ = False

-- unr :: Set.Set Variable -> K.KindEnv -> T.Type -> Bool
-- unr s kEnv (T.Semi _ t u) = unr s kEnv t || unr s kEnv u
-- unr s kEnv (T.Rec _ (Bind _ a k t)) = unr s (Map.insert a k kEnv) t
-- unr s kEnv (T.Labelled _ (T.Choice _) m) = all (unr s kEnv) (Map.elems m)
-- unr s kEnv (T.Var _ a) = Set.notMember a s && Map.member a kEnv
-- unr _ _ _ = False

