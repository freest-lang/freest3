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
( Visited
, GNFState
, convertToGNF
, initial
, toGNF
, freshVar
, insertProduction
, getGrammar
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Validation.Grammar
import           Syntax.Types
import           Syntax.Kinds

-- The state of the translation to GNF

type Visited = Map.Map Type TypeVar

type GNFState = State (Grammar, Visited, Int)

-- State manipulating functions

initial :: (Grammar, Visited, Int)
initial = (Map.empty, Map.empty, 0)

freshVar :: GNFState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n+1))
  return $ "_x" ++ show n

lookupVisited :: Type -> GNFState (Maybe TypeVar)
lookupVisited t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> GNFState ()
insertVisited t x =
  modify (\(p, v, n) -> (p, Map.insert t x v, n))

getGrammar :: GNFState Grammar
getGrammar = do
  (p, _, _) <- get
  return p

getProductions :: TypeVar -> GNFState (Map.Map Label [TypeVar])
getProductions x = do
  p <- getGrammar
  return $ p Map.! x

member :: TypeVar -> GNFState Bool
member x = do
  p <- getGrammar
  return $ Map.member x p

insertProduction :: TypeVar -> Label -> [TypeVar] -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertGrammar :: TypeVar -> (Map.Map Label [TypeVar]) -> GNFState ()
insertGrammar x m = insertGrammar' x (Map.assocs m)

insertGrammar' :: TypeVar -> [(Label, [TypeVar])] -> GNFState ()
insertGrammar' x [] = return ()
insertGrammar' x ((l, w):as) = do
  insertProduction x l w
  insertGrammar' x as

replaceInGrammar :: [TypeVar] -> TypeVar -> GNFState ()
replaceInGrammar w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: Eq a => [a] -> a -> [a] -> [a]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to GNF

convertToGNF :: (Grammar, Visited, Int) -> Type -> (TypeVar, (Grammar, Visited, Int))
-- Assume: t is a session type different from Skip
convertToGNF state t = (x, state')
  where ([x], state') = runState (toGNF0 t) state

toGNF0 :: Type -> GNFState [TypeVar]
toGNF0 t = do
  w <- toGNF t
  y <- freshVar
  insertProduction y (MessageLabel In UnitType) w
  return [y]

toGNF :: Type -> GNFState [TypeVar]
toGNF t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGNF' t
    Just x ->  return [x]

toGNF' :: Type -> GNFState [TypeVar]
toGNF' Skip =
  return []
toGNF' (Message p b) = do
  y <- freshVar
  insertProduction y (MessageLabel p b) []
  return [y]
toGNF' (Var a) = do -- This is a free variable
  y <- freshVar
  insertProduction y (VarLabel a) []
  return [y]
toGNF' (Semi (Choice p m) u) = do
  xs <- toGNF (Choice p m)
  ys <- toGNF u
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
toGNF' (Semi t u) = do
  xs <- toGNF t
  ys <- toGNF u
  return $ xs ++ ys
toGNF' (Choice p m) = do
  y <- freshVar
  assocsToGNF y p (Map.assocs m)
  return [y]
toGNF' (Rec b t) = do
  y <- freshVar
  let u = rename (Rec b t) y -- On the fly alpha conversion
  insertVisited u y
  (z:zs) <- toGNF (unfold u)
  replaceInGrammar (z:zs) y
  return [z]

assocsToGNF :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> GNFState ()
assocsToGNF _ _ [] = return ()
assocsToGNF y p ((l, t):as) = do
  w <- toGNF t
  insertProduction y (ChoiceLabel p l) w
  assocsToGNF y p as
