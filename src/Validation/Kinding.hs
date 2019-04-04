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
( checkAgainst
, synthetise
, synthetiseTS
, isSessionType
, un
, lin
) where

import           Parse.Lexer (Pos, position)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import           Validation.Contractive
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Debug.Trace

{-
class Kinding a where
  synthetise :: a -> FreestState Kind
  checkAgains :: Kind -> a -> FreestState ()
-}

-- Returns the kind of a given type
synthetise :: Type -> FreestState Kind
-- Session types
synthetise (Skip p) =
  return $ Kind p Session Un
synthetise (Message p _ _) =
  return $ Kind p Session Lin
synthetise (Choice p _ m) = do
  mapM_ (checkAgainst (Kind p Session Lin)) (Map.elems m)
  return $ Kind p Session Lin
synthetise (Semi p t u) = do
  m <- checkAgainstSession t
  n <- checkAgainstSession u
  return $ Kind p Session (max m n)
-- Functional
synthetise (Basic p _) =
  return $ Kind p Functional Un
synthetise (Fun p m t u) = do
  synthetise t
  synthetise u
  return $ Kind p Functional m
synthetise (PairType _ t u) = do
  kt <- synthetise t
  ku <- synthetise u
  return $ lub kt ku
synthetise (Datatype p m) = do
  ks <- mapM synthetise (Map.elems m)
  let Kind _ _ n = foldr1 lub ks
  return $ Kind p Functional n
synthetise (Rec p x@(TBindK _ _ k) t) = do
  checkContractive t
  y <- freshVar
  let b = TBind p y
  addToKenv b k
  k <- synthetise $ subs (TypeVar p y) x t -- On the fly α-conversion
  removeFromKenv b
  return k
-- Session or functional
synthetise (TypeVar p x) =
  let bind = TBind p x in
  getFromKenv bind >>= \case
    Just k ->
      return k
    Nothing -> do
      addError p ["Type variable not in scope:", styleRed x]
      let k = top p
      addToKenv bind k
      return k
-- Type operators
synthetise (Name p c) =
  let bind = TBind p c in
  getFromTenv bind >>= \case
    Just (k, _) ->
      return k
    Nothing -> do
      addError p ["Type name not in scope:", styleRed c]
      let k = top p
      addToTenv bind k $ omission p
      return k
synthetise (Dualof p t) = do
  m <- checkAgainstSession t
  return $ Kind p Session m

-- Check whether a given type is of a session kind. In either case
-- return the multiplicity of the kind of the type
checkAgainstSession :: Type -> FreestState Multiplicity
checkAgainstSession t = do
  (Kind _ k m) <- synthetise t
  when (k /= Session) $
    addError (position t) ["Expecting a session type; found", styleRed $ show t]
  return m

-- Check a type against a given kind
checkAgainst :: Kind -> Type -> FreestState ()
checkAgainst k (Rec p x t) = do
  checkContractive t
  y <- freshVar
  let b = TBind p y
  addToKenv b (Kind p Session Un)
  checkAgainst k $ subs (TypeVar p y) x t -- On the fly α-conversion
  removeFromKenv b
checkAgainst k1 t = do
  k2 <- synthetise t
  when (not (k2 <: k1)) $
    addError (position k1) ["Expecting kind", styleRed $ show k1,
                            "to be a sub-kind of", styleRed $ show k2]

synthetiseTS :: TypeScheme -> FreestState Kind
synthetiseTS (TypeScheme p bs t) = do
--  resetKEnv -- TODO: really?
  mapM_ (\(TBindK p x k) -> addToKenv (TBind p x) k) bs
  synthetise t

-- Determine whether a given type is linear
lin :: TypeScheme -> FreestState Bool
lin = mult Lin

-- Determine whether a given type is unrestricted
un :: TypeScheme -> FreestState Bool
un = mult Un

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> TypeScheme -> FreestState Bool
mult m1 t = do
  (Kind _ _ m2) <- synthetiseTS t
  return $ m2 == m1

-- Assumes the type is well formed
isSessionType :: KindEnv -> TypeEnv -> Type -> Bool
  -- Session types
isSessionType _ _ (Skip _)          = True
isSessionType _ _ (Semi _ _ _)      = True
isSessionType _ _ (Message _ _ _)   = True
isSessionType _ _ (Choice _ _ _)    = True
isSessionType kenv tenv (Rec _ _ t) = isSessionType kenv tenv t
  -- Functional or session
isSessionType kenv _ (TypeVar p x)  = Map.member (TBind p x) kenv
  -- Type operators
isSessionType _ _ (Dualof _ _)      = True
isSessionType kenv tenv (Name p c)  = isSession $ fst $ tenv Map.! (TBind p c)
  -- Otherwise: Functional types
isSessionType _ _ _                 = False
