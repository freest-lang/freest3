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

module Validation.Kinding
( Kind (..)
, checkAgainst
, kinding
, un
, lin 
, kindOfType -- test
, kindOfScheme -- test
, isWellFormed -- test
) where

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Utils.Errors
import           Utils.FreestState
import           Control.Monad.State
import           Validation.Contractive
import qualified Data.Map.Strict as Map

-- Returns the kind of a given type scheme -- TODO: type schemes do not have kinds
kinding :: TypeScheme -> FreestState Kind
kinding (TypeScheme _ bs t) = do
  -- TODO: addToKenv -> addBindsLToKenv
  foldM_ (\_ (KBind p x k) -> addToKenv (Bind p x) k) () bs
  synthetize t

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
  kt <- synthetize t 
  ku <- synthetize u
  m <- checkSessionKind t kt
  n <- checkSessionKind u ku
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
  return $ max kt ku
synthetize (Datatype p m) = do
  ks <- mapM synthetize (Map.elems m)
  let Kind _ _ n = maximum ks
  return $ Kind p Functional n
synthetize (Rec p x t) = do
  checkContractive t
  y <- freshVar
  let b = Bind p y
  addToKenv b (Kind p Session Un)
  k <- synthetize $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
  return k
-- Session or functional
synthetize (Var p v) = do
  let bind = Bind p v
  b <- kenvMember bind
  if b then
    getKind bind
  else do
    addError p ["Variable not in scope: ", styleRed v]
    let k = topKind p
    addToKenv bind k
    return k

-- Check whether a given kind is session; issue an error if not. In
-- either case return the multiplicity
checkSessionKind :: Type -> Kind -> FreestState Multiplicity
checkSessionKind t k@(Kind _ p m)
  | p == Session = return $ m
  | otherwise    = do
      addError (position t) ["Expecting type", styleRed $ show t,
                  "to be a session type; found kind", styleRed $ show k]
      return $ m

-- Check whether a given type has a given kind
checkAgainst :: Kind -> Type -> FreestState ()
checkAgainst k (Rec p x t) = do
  checkContractive t
  y <- freshVar
  let b = Bind p y
  addToKenv b (Kind p Session Un)
  checkAgainst k $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
checkAgainst k t = do
  k' <- synthetize t
  checkSubkind (position t) k' k

-- Checks whether a given kind is a sub kind of another;
-- gives an error message if it isn't
checkSubkind :: Pos -> Kind -> Kind -> FreestState ()
checkSubkind p k1 k2
  | k1 <= k2  = return ()
  | otherwise =
      addError p ["Expecting kind", styleRed $ show k1, "to be a sub-kind of kind of kind", styleRed $ show k2]

-- Determines whether a given type is of a given multiplicity
mult :: Multiplicity -> Type -> FreestState Bool
mult m t = do
  (Kind _ _ m') <- synthetize t
  return $ m' == m
      
-- Determines whether a given type is linear or not
lin :: Type -> FreestState Bool
lin = mult Lin

-- Determines whether a given type is unrestricted or not
un :: Type -> FreestState Bool
un = mult Un

-- Used to insert in the kinding environment when an error is found
topKind :: Pos -> Kind
topKind p = Kind p Functional Lin

-- For TESTS only, from here on

kindOfType :: KindEnv -> Type -> Kind
kindOfType k t =
  let s = (initialState  "") in
  evalState (synthetize t) (s {kindEnv=k})

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (kinding t) (initialState "")

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let s = initialState "" in
  let s1 = execState (synthetize t) (s {kindEnv=k}) in
    null (errors s1)
