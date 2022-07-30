{-|
Module      :  Validation.Kinding
Description :  Check the type formation
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Kinding
  ( synthetise
  , checkAgainst
  , checkAgainstSession
  , un
  , lin
  )
where

import           Syntax.Base
import qualified Syntax.Type as T
import qualified Syntax.Kind as K
import           Validation.Contractive
import           Validation.Subkind ( (<:), join )
import           Util.FreestState
import           Util.Error


import           Control.Monad ( unless )
import qualified Control.Monad.State as S
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Exported Functions: Top-level definitions of those defined in this module

synthetise :: K.KindEnv -> T.Type -> FreestState K.Kind
synthetise kenv = synthetise'  (Map.keysSet kenv) kenv

checkAgainst :: K.KindEnv -> K.Kind -> T.Type -> FreestState K.Kind
checkAgainst kenv = checkAgainst' (Map.keysSet kenv) kenv

checkAgainstSession :: K.KindEnv -> T.Type -> FreestState K.Kind
checkAgainstSession kenv = checkAgainstSession' (Map.keysSet kenv) kenv

-- Kinding
-- Returns the kind of a given type
synthetise' :: K.PolyVars -> K.KindEnv -> T.Type -> FreestState K.Kind
-- Functional types
synthetise' _ _ (T.Int    p) = return $ K.ut p
synthetise' _ _ (T.Char   p) = return $ K.ut p
synthetise' _ _ (T.Bool   p) = return $ K.ut p
synthetise' _ _ (T.Unit   p) = return $ K.ut p
synthetise' _ _ (T.String p) = return $ K.ut p
synthetise' s kEnv (T.Arrow p m t u) =
  synthetise' s kEnv t >> synthetise' s kEnv u $> K.Kind p (typeToKindMult m) K.Top
                                                          -- K.Top
synthetise' s kEnv (T.Pair p t u) = do
  (K.Kind _ mt _) <- synthetise' s kEnv t
  (K.Kind _ mu _) <- synthetise' s kEnv u
  return $ K.Kind p (join mt mu) K.Top
synthetise' s kEnv (T.Almanac p T.Variant m) = do
  ks <- tMapM (synthetise' s kEnv) m
  let K.Kind _ n _ = foldr1 join ks
  return $ K.Kind p n K.Top
-- Shared session types
synthetise' s kEnv (T.Rec p1 (Bind _ a k (T.Semi p2 (T.Message p3 pol t) (T.Var p4 tVar))))
  | K.isUn k && a == tVar = do
    checkAgainstSession' s (Map.insert a k kEnv) (T.Semi p2 (T.Message p3 pol t) (T.Var p4 tVar))
    return $ K.us p1
synthetise' s kEnv (T.Rec p1 (Bind p2 a k (T.Almanac p3 (T.Choice v) m)))
  | K.isUn k && all (\t -> case t of (T.Var _ a') -> a == a' ; _ -> False) m = do
    return $ K.us p1
-- Session types
synthetise' _ _    (T.Skip p    ) = return $ K.us p
synthetise' s kEnv (T.Semi p t u) = do
  (K.Kind _ mt _) <- checkAgainstSession' s kEnv t
  (K.Kind _ mu _) <- checkAgainstSession' s kEnv u
  return $ K.Kind p (join mt mu) K.Session
synthetise' s kEnv (T.Message p _ t) = checkAgainst' s kEnv (K.lt p) t $> K.ls p -- HO CFST
synthetise' s kEnv (T.Almanac p (T.Choice _) m) =
  tMapM_ (checkAgainst' s kEnv (K.ls p)) m $> K.ls p
-- Session or functional
synthetise' s kEnv (T.Rec _ (Bind _ a k t)) =
  checkContractive s a t >> checkAgainst' s (Map.insert a k kEnv) k t $> k
synthetise' s kEnv (T.Forall _ (Bind p a k t)) = do
  (K.Kind _ m _) <- synthetise' (Set.insert a s) (Map.insert a k kEnv) t
  return $ K.Kind p m K.Top
synthetise' _ kEnv (T.Var p a) = case kEnv Map.!? a of
  Just k -> return k
  Nothing -> addError (TypeVarNotInScope p a) $> omission p
-- Type operators
synthetise' _ kEnv t@(T.CoVar p a) =
  case kEnv Map.!? a of
    Just k -> S.when (not $ k <: K.ls p)
            (addError (CantMatchKinds p k (K.ls p) t)) $> K.ls p
    Nothing -> addError (TypeVarNotInScope p a) $> omission p

synthetise' _ _ t@T.Dualof{} = internalError "Validation.Kinding.synthetise'" t

-- Check the contractivity of a given type; issue an error if not
checkContractive :: K.PolyVars -> Variable -> T.Type -> FreestState ()
checkContractive s a t = let p = getSpan t in
  unless (contractive s a t) $ addError (TypeNotContractive p t a)

-- Check a type against a given kind

checkAgainst' :: K.PolyVars -> K.KindEnv -> K.Kind -> T.Type -> FreestState K.Kind
checkAgainst' s kEnv expected t = do
  actual <- synthetise' s kEnv t
  S.when (not $ actual <: expected)
    (addError (CantMatchKinds (getSpan t) expected actual t))
  $> expected

-- Check whether a given type is of a session kind. In any case return the
-- kind of the type. This is a refined version of checkAgainst for a better error messages
checkAgainstSession' :: K.PolyVars -> K.KindEnv -> T.Type -> FreestState K.Kind
checkAgainstSession' s kEnv t = do
  k@(K.Kind _ _ p) <- synthetise' s kEnv t
  S.when (p /= K.Session) (addError (ExpectingSession (getSpan t) t k)) 
  return k

-- Determine whether a given type is unrestricted
un :: T.Type -> FreestState Bool
un = mult K.Un

-- Determine whether a given type is linear
lin :: T.Type -> FreestState Bool
lin = mult K.Lin

-- Determine whether a given type is of a given multiplicity
mult :: K.Multiplicity -> T.Type -> FreestState Bool
mult m1 t = do
  (K.Kind _ m2 _) <- synthetise' Set.empty Map.empty t
  return $ m2 == m1

-- Type to kind multiplicity
typeToKindMult :: Multiplicity -> K.Multiplicity
typeToKindMult Lin = K.Lin
typeToKindMult Un = K.Un
