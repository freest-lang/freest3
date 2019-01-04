{-# LANGUAGE NoMonadFailDesugaring #-}
{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.TypeToGrammar
( TransState
, convertToGrammar
, initial
, toGrammar
, freshVar
, insertProduction
, getGrammar
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Validation.Grammar
import           Syntax.Types
import           Syntax.Kinds

-- The state of the translation to grammars

type Visited = Map.Map Type TypeVar

type Trans = (Productions, Visited, Int)

type TransState = State Trans

-- State manipulating functions

initial :: Trans
initial = (Map.empty, Map.empty, 0)

freshVar :: TransState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n+1))
  return $ "_x" ++ show n

lookupVisited :: Type -> TransState (Maybe TypeVar)
lookupVisited t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> TransState ()
insertVisited t x =
  modify (\(p, v, n) -> (p, Map.insert t x v, n))

getGrammar :: TransState Productions
getGrammar = do
  (p, _, _) <- get
  return p

getProductions :: TypeVar -> TransState Transitions
getProductions x = do
  p <- getGrammar
  return $ p Map.! x

member :: TypeVar -> TransState Bool
member x = do
  p <- getGrammar
  return $ Map.member x p

insertProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertGrammar :: TypeVar -> Transitions -> TransState ()
insertGrammar x m = insertGrammar' x (Map.assocs m)

insertGrammar' :: TypeVar -> [(Label, [TypeVar])] -> TransState ()
insertGrammar' x [] = return ()
insertGrammar' x ((l, w):as) = do
  insertProduction x l w
  insertGrammar' x as

replaceInGrammar :: [TypeVar] -> TypeVar -> TransState ()
replaceInGrammar w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: [TypeVar] -> TypeVar -> [TypeVar] -> [TypeVar]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to context-free grammars

convertToGrammar :: Trans -> Type -> (TypeVar, Trans)
convertToGrammar state t = (x, state')
  where ([x], state') = runState (toGrammar0 t) state

toGrammar0 :: Type -> TransState [TypeVar]
toGrammar0 t = do
  w <- toGrammar t
  y <- freshVar
  insertProduction y (MessageLabel In UnitType) w
  return [y]

toGrammar :: Type -> TransState [TypeVar]
toGrammar t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGrammar' t
    Just x ->  return [x]

toGrammar' :: Type -> TransState [TypeVar]
toGrammar' Skip =
  return []
toGrammar' (Message p b) = do
  y <- freshVar
  insertProduction y (MessageLabel p b) []
  return [y]
toGrammar' (Var a) = do -- This is a free variable
  y <- freshVar
  insertProduction y (VarLabel a) []
  return [y]
toGrammar' (Semi (Choice p m) u) = do
  xs <- toGrammar (Choice p m)
  ys <- toGrammar u
  if null xs
  then return ys
--  else if null ys
--  then return xs
  --else if t == (Rec b t)
  --then return $ xs ++ ys
  else do
    let (x:xs') = xs
    b <- member x
    if b then do
      m <- getProductions x
      insertGrammar x (Map.map (++ xs' ++ ys) m)
      return [x]
    else
      return $ xs ++ ys -- E.g., rec x. !Int;(x;x)
toGrammar' (Semi t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar' (Choice p m) = do
  y <- freshVar
  assocsToGrammar y p (Map.assocs m)
  return [y]
toGrammar' (Rec b t) = do
  y <- freshVar
  let u = rename (Rec b t) y -- On the fly alpha conversion
  insertVisited u y
  (z:zs) <- toGrammar (unfold u)
  replaceInGrammar (z:zs) y
  return [z]

assocsToGrammar :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> TransState ()
assocsToGrammar _ _ [] = return ()
assocsToGrammar y p ((l, t):as) = do
  w <- toGrammar t
  insertProduction y (ChoiceLabel p l) w
  assocsToGrammar y p as
