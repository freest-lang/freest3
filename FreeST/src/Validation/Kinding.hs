{-|
Module      :  Kinding
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Kinding
( synthetise
, checkAgainst
, checkAgainstSession
, synthetiseTS
, un
, lin
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Base
import           Syntax.Show()
import           Validation.Contractive
import           Utils.FreestState
import           Utils.Errors()
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map

-- Returns the kind of a given type
synthetise :: KindEnv -> Type -> FreestState Kind
-- Functional types
synthetise _ (Basic p _) =
  return $ Kind p Functional Un
synthetise kEnv (Fun p m t u) = do
  synthetise kEnv t
  synthetise kEnv u
  return $ Kind p Functional m
synthetise kEnv (PairType _ t u) = do
  kt <- synthetise kEnv t
  ku <- synthetise kEnv u
  return $ join kt ku
synthetise kEnv (Datatype p m) = do
  ks <- tMapM (synthetise kEnv) m
  let Kind _ _ n = foldr1 join ks
  return $ Kind p Functional n
  -- Session types
synthetise _ (Skip p) =
  return $ Kind p Session Un
synthetise kEnv (Semi p t u) = do
  m <- checkAgainstSession kEnv t
  n <- checkAgainstSession kEnv u
  return $ Kind p Session (max m n)
synthetise _ (Message p _ _) =
  return $ Kind p Session Lin
synthetise kEnv (Choice p _ m) = do
  tMapM_ (checkAgainst kEnv (Kind p Session Lin)) m
  return $ Kind p Session Lin
-- Session or functional
synthetise kEnv (Rec _ (TypeVarBind _ x k) t) = do
  -- let (Rec _ (TypeVarBind _ x k) u) = rename t -- On the fly α-conversion
--  checkContractive kEnv t
  
  -- checkContractive x t
  synthetise (Map.insert x k kEnv) t
synthetise kEnv (TypeVar p x) =
  case kEnv Map.!? x of
    Just k -> do
      return k
    Nothing -> do
      addError p [Error "Type variable not in scope:", Error x]
      return $ omission p
-- Type operators
synthetise _ (TypeName p a) =
  getFromTEnv a >>= \case
    Just (k, _) ->
      return k
    Nothing -> do
      addError p [Error "Type name not in scope:", Error a]
      addToTEnv a (omission p) (omission p)
      return $ omission p
synthetise kEnv (Dualof p t) = do
  m <- checkAgainstSession kEnv t
  return $ Kind p Session m

-- Check whether a given type is of a session kind. In any case return
-- the multiplicity of the kind of the type
checkAgainstSession :: KindEnv -> Type -> FreestState Multiplicity
checkAgainstSession kEnv t = do
  k@(Kind _ p m) <- synthetise kEnv t
  S.when (p /= Session) $
    addError (position t) [Error "Expecting a session type\n",
                           Error "\t found type", Error t, Error "of kind", Error k]
  return m

-- Check a type against a given kind
checkAgainst :: KindEnv -> Kind -> Type -> FreestState ()
-- checkAgainst kEnv k (Rec _ (TypeVarBind p x _) t) = do
--   checkContractive kEnv t
--   checkAgainst (Map.insert x (Kind p Session Un) kEnv) k t
checkAgainst kEnv expected t = do
  actual <- synthetise kEnv t
  S.when (not (actual <: expected)) $
    addError (position t)
      [Error "Couldn't match expected kind", Error expected,
       Error "\n\t with actual kind", Error actual,
       Error "\n\t for type", Error t]

synthetiseTS :: KindEnv -> TypeScheme -> FreestState Kind
synthetiseTS kEnv (TypeScheme _ bs t) = synthetise insertBinds t
  where insertBinds = foldr (\(TypeVarBind _ x k) env -> Map.insert x k env) kEnv bs

-- Determine whether a given type is unrestricted
un :: TypeScheme -> FreestState Bool
un = mult Un

-- Determine whether a given type is linear
lin :: TypeScheme -> FreestState Bool
lin = mult Lin

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> TypeScheme -> FreestState Bool
mult m1 s = do
  (Kind _ _ m2) <- synthetiseTS Map.empty s
  return $ m2 == m1
