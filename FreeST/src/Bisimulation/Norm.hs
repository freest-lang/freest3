{-# LANGUAGE FlexibleInstances #-}
module Bisimulation.Norm
  ( isNormed
  , norm
  , normUsingMap
  , nonterminals
  , reducesNorm
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Function (on)
import Data.List
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set
import Bisimulation.State
import Prelude hiding (Word, log)
import SimpleGrammar.Grammar
import Syntax.Base

nonterminals :: Grammar -> Set.Set Variable
nonterminals (Grammar _ productions) = Set.union (Map.keysSet productions) (Set.fromList $ concatMap (concatMap snd . Map.toList) (Map.elems productions))

-- 1 Given a Grammar and a word indicates if the word is normed
isNormed :: Grammar -> Word -> Bool
isNormed g w = Data.Maybe.isJust (norm g w)

-- 2 Given a Grammar and a word returns the norm of the word
class Normalizable a where
  norm :: Grammar -> a -> IOF

instance Normalizable Word where
  norm g w = evalState (normUsingMap Set.empty w) initState
    where
      initState = TState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

instance Normalizable Variable where
  norm g v = norm g [v]

-- Calculates the norm using the NormMap
-- Receive a grammar, a word, a map and an array of "Variables" (variables already visited)
-- Returns a tuple with the values of the norm, the variables already visited and the NormMap
normUsingMap :: Set.Set Variable -> Word -> GlobalState IOF
normUsingMap _ [] = return (Just 0)
normUsingMap visitedVars w@(v : vs) = do
  -- troco a ordem e ponho o visitedVars como parametro
  map <- gets normMap
  case Map.lookup v map of
    Just nVar -> do
      nTail <- normUsingMap visitedVars vs
      return (liftA2 (+) nTail nVar)
    Nothing -> do
      if v `elem` visitedVars
        then do
          return Nothing
        else do
          (Grammar _ prods) <- gets grammar
          wordNorm visitedVars prods w

wordNorm :: Set.Set Variable -> Productions -> Word -> GlobalState IOF
wordNorm visitedVars prods (v : vs) = do
  map <- gets normMap
  let newVisitedVars = Set.insert v visitedVars
      trans = transitions v prods
      toVisit = Map.elems trans -- ver se da para fazer sem listas/ noa consegui
  if [] `elem` toVisit
    then do
      let nVar = Just 1
      updateNormMap (Map.insert v nVar map)
      nTail <- normUsingMap newVisitedVars vs
      return (liftA2 (+) nVar nTail)
    else do
      nVar <-
        foldM
          ( \acc w -> do
              a <- normUsingMap newVisitedVars w
              return (minWithInf acc a)
          )
          Nothing
          toVisit
      map <- gets normMap
      let nVar' = liftA2 (+) nVar (Just 1)
      updateNormMap (Map.insert v nVar' map)
      nTail <- normUsingMap newVisitedVars vs
      return (liftA2 (+) nVar' nTail)

-- Auxiliary function of normUsingMap
-- normUsingMapAux ::  [Word] -> NormState IOF
-- normUsingMapAux [] = return Nothing
-- normUsingMapAux  (w:ws) = do
-- a <- normUsingMap w
--  b <- normUsingMapAux  ws
--  return (minWithInf a b)

-----------------------------------------------------------------------------------------------------------------------------

-- 3 Given a Grammar and a word returns the seminorm of the word
seminorm :: Grammar -> Word -> Int
seminorm g w = evalState (seminormUsingMap Set.empty w) initState
  where
    initState = TState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Calculates the seminorm using the NormMap
-- Receive a grammar, a word, a map and an array of "Variables" (variables already visited)
-- Returns a tuple with the values of the norm, the variables already visited and the NormMap
seminormUsingMap :: Set.Set Variable -> Word -> GlobalState Int
seminormUsingMap _ [] = return 0
seminormUsingMap visitedVars (x : xs) = do
  result <- normUsingMap visitedVars [x]
  case result of
    Nothing -> return 0
    Just y -> do
      result' <- seminormUsingMap visitedVars xs
      return (y + result')

---------------------------------------------------------------------------------------------------------------------

-- 4 Calculates the k-th term in the canonical seminorm-reducing sequence of a word
-- requires 0 <= i <= seminorm g w
yk :: Grammar -> Word -> Int -> Word
yk g w i = evalState (ykUsingMap Set.empty i w) initState
  where
    initState = TState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Calculates the  k-th term using the NormMap
-- Receive a grammar, a word, the k, a map and an array of "Variables" (variables already visited)
-- Returns a word
ykUsingMap :: Set.Set Variable -> Int -> Word -> GlobalState Word
ykUsingMap _ 0 w = return w
ykUsingMap visitedVars i (x : xs) = do
  result <- normUsingMap visitedVars [x]
  case result of
    Just n
      | n <= i -> ykUsingMap visitedVars (i - n) xs
      | n > i -> do
          map <- gets normMap
          (Grammar _ prods) <- gets grammar
          let trans = transitions x prods
              toVisit = Map.elems trans
              Just w = find (\word -> reducesNorm n (getNormWordFromMap word map)) toVisit
          ykUsingMap visitedVars (i - 1) (w ++ xs)

-----------------------------------------------------------------------------------------------------------------------------

-- 5 Calculate the valuation of a grammar
-- This function is not strictly necessary for the algorithm
valuation :: Grammar -> Int
valuation g@(Grammar _ p) = evalState (valuationUsingMap Set.empty words) initState
  where
    transList = Map.elems p
    words = concatMap Map.elems transList
    initState = TState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Auxiliary function of valuation
valuationUsingMap :: Set.Set Variable -> [Word] -> GlobalState Int
valuationUsingMap visitedVars = foldM (\acc w -> max acc <$> seminormUsingMap visitedVars w) 0

-----------------------------------------------------------------------------------------------------------------------------

-- Extra Functions
minWithInf :: IOF -> IOF -> IOF
minWithInf x Nothing = x
minWithInf Nothing x = x
minWithInf (Just x) (Just y) = Just (min x y)

reducesNorm :: Int -> Int -> Bool
reducesNorm x y = x == (y + 1)

getNormFromMap :: Variable -> NormMap -> Int
getNormFromMap x mp = case Map.lookup x mp of
  Just val -> case val of
    Nothing -> -1
    Just x -> x
  Nothing -> undefined

getNormWordFromMap :: Word -> NormMap -> Int
getNormWordFromMap [] _ = 0
getNormWordFromMap [x] mp = getNormFromMap x mp
getNormWordFromMap (x : xs) mp = getNormFromMap x mp + getNormWordFromMap xs mp
