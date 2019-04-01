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

{-# LANGUAGE LambdaCase #-}

module Validation.Kinding
( checkAgainst
, synthetize
, synthetizeTS
, isSessionType
, un
, lin
, kindOfType -- test
, kindOfScheme -- test
, isWellFormed -- test
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

-- Returns the kind of a given type
synthetize :: Type -> FreestState Kind
-- Session types
synthetize (Skip p) =
  return $ Kind p Session Un
synthetize (Message p _ _) =
  return $ Kind p Session Lin
synthetize (Choice p _ m) = do
  mapM_ (checkAgainst (Kind p Session Lin)) (Map.elems m)
  return $ Kind p Session Lin
synthetize (Semi p t u) = do
  m <- checkAgainstSession t
  n <- checkAgainstSession u
  return $ Kind p Session (max m n)
-- Functional
synthetize (Basic p _) =
  return $ Kind p Functional Un
synthetize (Fun p m t u) = do
  synthetize t
  synthetize u
  return $ Kind p Functional m
synthetize (PairType _ t u) = do
  kt <- synthetize t
  ku <- synthetize u
  return $ lub kt ku
synthetize (Datatype p m) = do
  ks <- mapM synthetize (Map.elems m)
  let Kind _ _ n = foldr1 lub ks
  return $ Kind p Functional n
synthetize (Rec p x@(TBindK _ _ k) t) = do
  checkContractive t
  y <- freshVar
  let b = TBind p y
  addToKenv b k
  k <- synthetize $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
  return k
-- Session or functional
synthetize (Var p x) =
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
synthetize (Name p c) =
  let bind = TBind p c in
  getFromTenv bind >>= \case
    Just (k, _) ->
      return k
    Nothing -> do
      addError p ["Type name not in scope:", styleRed c]
      let k = top p
      addToTenv bind k (TypeScheme p [] (Basic p UnitType))
      return k
synthetize (Dualof p t) = do
  m <- checkAgainstSession t
  return $ Kind p Session m
  
-- Check whether a given type is of a session kind. In either case
-- return the multiplicity of the kind of the type
checkAgainstSession :: Type -> FreestState Multiplicity
checkAgainstSession t = do
  (Kind _ k m) <- synthetize t
  when (k /= Session) $
    addError (position t) ["Expecting a session type; found", styleRed $ show t]
  return m

-- Checks a type against a given kind
checkAgainst :: Kind -> Type -> FreestState ()
checkAgainst k (Rec p x t) = do
  checkContractive t
  y <- freshVar
  let b = TBind p y
  addToKenv b (Kind p Session Un)
  checkAgainst k $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
checkAgainst k1 t = do
  k2 <- synthetize t
  when (not (k2 <: k1)) $
    addError (position k1) ["Expecting kind", styleRed $ show k1,
                            "to be a sub-kind of", styleRed $ show k2]

synthetizeTS :: TypeScheme -> FreestState Kind
synthetizeTS (TypeScheme _ ks t) = do
  resetKEnv
  mapM_ (\(TBindK p x k) -> addToKenv (TBind p x) k) ks
  synthetize t

-- Determines whether a given type is linear or not
lin :: Type -> FreestState Bool
lin = mult Lin

-- Determines whether a given type is unrestricted or not
un :: Type -> FreestState Bool
un = mult Un

-- Determines whether a given type is of a given multiplicity
mult :: Multiplicity -> Type -> FreestState Bool
mult m1 t = do
  (Kind _ _ m2) <- synthetize t
  return $ m2 == m1
      
-- For TESTS only, from here on

kindOfType :: KindEnv -> Type -> Kind
kindOfType k t =
  let s = (initialState  "") in
  evalState (synthetize t) (s {kindEnv=k})

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (synthetizeTS t) (initialState "")

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let s = initialState "" in
  let s1 = execState (synthetize t) (s {kindEnv=k}) in
    null (errors s1)

-- Assumes the type is well formed
isSessionType :: KindEnv -> TypeEnv -> Type -> Bool
  -- Session types
isSessionType _ _ (Skip _)          = True
isSessionType _ _ (Semi _ _ _)      = True
isSessionType _ _ (Message _ _ _)   = True
isSessionType _ _ (Choice _ _ _)    = True
isSessionType kenv tenv (Rec _ _ t) = isSessionType kenv tenv t
  -- Functional or session
isSessionType kenv _ (Var p x)      = Map.member (TBind p x) kenv
  -- Type operators
isSessionType _ _ (Dualof _ _)      = True
isSessionType kenv tenv (Name p c)  = isSessionType kenv tenv t
  where (_, TypeScheme _ [] t) = tenv Map.! (TBind p c) -- TODO: polymorphic type names
  -- Otherwise: Functional types
isSessionType _ _ _                 = False
