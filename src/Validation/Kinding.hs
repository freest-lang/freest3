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
, synthetiseTS
, un
, fromTypeVarBinds
) where

import           Syntax.Expressions
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Base
import           Validation.Contractive
import           Utils.FreestState
import           Utils.Errors
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map
import           Debug.Trace

-- Returns the kind of a given type
synthetise :: KindEnv -> Type -> FreestState Kind
  -- Session types
synthetise _ (Skip p) =
  return $ Kind p Session Un
synthetise _ (Message p _ _) =
  return $ Kind p Session Lin
synthetise kEnv (Choice p _ m) = do
  tMapM (checkAgainst kEnv (Kind p Session Lin)) m
  return $ Kind p Session Lin
synthetise kEnv (Semi p t u) = do
  m <- checkAgainstSession kEnv t
  n <- checkAgainstSession kEnv u
  return $ Kind p Session (max m n)
-- Functional
synthetise _ (Basic p _) =
  return $ Kind p Functional Un
synthetise kEnv v@(Fun p m t u) = do
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
synthetise kEnv (Rec p (TypeVarBind _ x k) t) = do
  checkContractive kEnv t
  synthetise (Map.insert x k kEnv) t
  -- y <- freshVar
  -- k' <- synthetise (Map.insert (TypeVarBind p y) k kEnv) $ subs (TypeVar p y) b t -- On the fly α-conversion
  -- return k'
-- Session or functional
synthetise kEnv (TypeVar p x) =
  case kEnv Map.!? x of
    Just k -> do
      return k
    Nothing -> do
      addError p ["Type variable not in scope:", styleRed $ show x]
      return (omission p)
-- Type operators
synthetise kEnv (TypeName p a) =
  getFromTEnv a >>= \case
    Just (k, _) ->
      return k
    Nothing -> do
      addError p ["Type name not in scope:", styleRed $ show a]
      let k = omission p
      addToTEnv a k $ omission p
      return k
synthetise kEnv (Dualof p t) = do
  m <- checkAgainstSession kEnv t
  return $ Kind p Session m

-- Check whether a given type is of a session kind. In any case return
-- the multiplicity of the kind of the type
checkAgainstSession :: KindEnv -> Type -> FreestState Multiplicity
checkAgainstSession kEnv t = do
  (Kind _ k m) <- synthetise kEnv t
  S.when (k /= Session) $
    addError (position t) ["Expecting a session type; found", styleRed $ show t]
  return m

-- Check a type against a given kind
checkAgainst :: KindEnv -> Kind -> Type -> FreestState ()
checkAgainst kEnv k (Rec _ (TypeVarBind p x _) t) = do
  checkContractive kEnv t
  checkAgainst (Map.insert x (Kind p Session Un) kEnv) k t
  -- checkAgainst (Map.insert (TypeVarBind p y) (Kind p Session Un) kEnv) k $ subs (TypeVar p y) x t -- On the fly α-conversion
checkAgainst kEnv expected t = do
  actual <- synthetise kEnv t
  S.when (not (actual <: expected)) $
    addError (position t) ["Couldn't match expected kind", styleRed $ show expected, "\n",
                            "\t with actual kind", styleRed $ show actual, "\n",
                            "\t for type", styleRed $ show t]

synthetiseTS :: KindEnv -> TypeScheme -> FreestState Kind
synthetiseTS kEnv (TypeScheme _ bs t) = synthetise insertBinds t
  where insertBinds = foldr (\(TypeVarBind _ x k) env -> Map.insert x k env) kEnv bs

fromTypeVarBinds :: [TypeVarBind] -> KindEnv
fromTypeVarBinds = foldr (\(TypeVarBind _ x k) env -> Map.insert x k env) Map.empty

-- Determine whether a given type is unrestricted
un :: TypeScheme -> FreestState Bool
un = mult Un

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> TypeScheme -> FreestState Bool
mult m1 s = do
  (Kind _ _ m2) <- synthetiseTS Map.empty s
  return $ m2 == m1
