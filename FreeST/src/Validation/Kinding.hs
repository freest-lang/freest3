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

{-# LANGUAGE NoMonadFailDesugaring #-}

module Validation.Kinding
  ( synthetise
  , checkAgainst
  , checkAgainstSession
  , un
  , lin
  )
where

import           Data.Functor
import           Syntax.Base
import           Syntax.TypeVariable
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Validation.Contractive
import           Validation.Subkind             ( (<:)
                                                , join
                                                )
import           Util.FreestState
import           Util.Error                     ( internalError )
import           Util.Error
import           Control.Monad                  ( unless )
import qualified Control.Monad.State           as S
import qualified Data.Map.Strict               as Map
import qualified Data.Set               as Set


-- Exported Functions: Top-level definitions of those defined in this module

synthetise :: K.KindEnv -> T.Type -> FreestState K.Kind
synthetise kenv = synthetise'  (Map.keysSet kenv) kenv

checkAgainst :: K.KindEnv -> K.Kind -> T.Type -> FreestState K.Kind
checkAgainst kenv = checkAgainst' (Map.keysSet kenv) kenv

checkAgainstSession :: K.KindEnv -> T.Type -> FreestState ()
checkAgainstSession kenv = checkAgainstSession' (Map.keysSet kenv) kenv


-- Kinding
-- Returns the kind of a given type
synthetise' :: K.PolyVars -> K.KindEnv -> T.Type -> FreestState K.Kind
-- Functional types
synthetise' _ _ (T.Int    p) = return $ K.Kind p K.Message Un
synthetise' _ _ (T.Char   p) = return $ K.Kind p K.Message Un
synthetise' _ _ (T.Bool   p) = return $ K.Kind p K.Message Un
synthetise' _ _ (T.Unit   p) = return $ K.Kind p K.Message Un
synthetise' _ _ (T.String p) = return $ K.Kind p K.Message Un
synthetise' s kEnv (T.Arrow p m t u) = -- do
  synthetise' s kEnv t >> synthetise' s kEnv u $> K.Kind p K.Top m
synthetise' s kEnv (T.Pair p t u) = do
  (K.Kind _ _ mt) <- synthetise' s kEnv t
  (K.Kind _ _ mu) <- synthetise' s kEnv u
  return $ K.Kind p K.Top (join mt mu)
synthetise' s kEnv (T.Variant p m) = do
  ks <- tMapM (synthetise' s kEnv) m
  let K.Kind _ _ n = foldr1 join ks
  return $ K.Kind p K.Top n
  -- Session types
synthetise' _ _    (T.Skip p    ) = return $ K.su p
synthetise' s kEnv (T.Semi p t u) = do
  checkAgainstSession' s kEnv t
  checkAgainstSession' s kEnv u
  return $ K.sl p
synthetise' s kEnv (T.Message p _ t) = checkAgainst' s kEnv (K.ml p) t $> K.sl p
synthetise' s kEnv (T.Choice p _ m) =
  tMapM_ (checkAgainst' s kEnv (K.sl p)) m $> K.sl p
-- Session or functional
synthetise' s kEnv (T.Rec _ (K.Bind _ a k t)) =
  checkContractive s a t >> checkAgainst' s (Map.insert a k kEnv) k t $> k
synthetise' s kEnv (T.Forall _ (K.Bind p a k t)) = do
  (K.Kind _ _ m) <- synthetise' (Set.insert a s) (Map.insert a k kEnv) t
  return $ K.Kind p K.Top m
synthetise' _ kEnv (T.Var p a) = case kEnv Map.!? a of
  Just k -> return k
  Nothing -> addError (TypeVarNotInScope p a) $> omission p
-- Type operators
synthetise' _ kEnv t@(T.CoVar p a) =
  case kEnv Map.!? a of
    Just k -> S.when (not $ k <: K.sl p)
            (addError (CantMatchKinds p k (K.sl p) t)) $> K.sl p
    Nothing -> addError (TypeVarNotInScope p a) $> omission p

synthetise' _ _ t@T.Dualof{} = internalError "Validation.Kinding.synthetise'" t

-- Check the contractivity of a given type; issue an error if not
checkContractive :: K.PolyVars -> TypeVar -> T.Type -> FreestState ()
checkContractive s a t = let p = pos t in
  unless (contractive s a t) $ addError (TypeNotContractive p t a)

-- Check a type against a given kind

checkAgainst' :: K.PolyVars -> K.KindEnv -> K.Kind -> T.Type -> FreestState K.Kind
checkAgainst' s kEnv expected t = do
  actual <- synthetise' s kEnv t
  S.when (not $ actual <: expected)
    (addError (CantMatchKinds (pos t) expected actual t))
  $> expected

-- Check whether a given type is of a session kind. In any case return the
-- multiplicity of the kind of the type. This is a refined version of
-- checkAgainst for a better error messages
checkAgainstSession' :: K.PolyVars -> K.KindEnv -> T.Type -> FreestState ()
checkAgainstSession' s kEnv t = do
  k@(K.Kind _ p _) <- synthetise' s kEnv t
  S.when (p /= K.Session) $ let p = pos t in
    addError (ExpectingSession p t k)

-- Determine whether a given type is unrestricted
un :: T.Type -> FreestState Bool
un = mult Un

-- Determine whether a given type is linear
lin :: T.Type -> FreestState Bool
lin = mult Lin

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> T.Type -> FreestState Bool
mult m1 t = do
  (K.Kind _ _ m2) <- synthetise' Set.empty Map.empty t
  return $ m2 == m1
