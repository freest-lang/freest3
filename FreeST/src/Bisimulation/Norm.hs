{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
isNormed g w = case norm g w of Normed _ -> True ; _ -> False

-- 2 Given a Grammar and a word returns the norm of the word
class Normalizable a where
  norm :: Grammar -> a -> Norm

instance Normalizable Word where
  norm g w = evalState (normUsingMap Set.empty w) initState
    where
      initState = BisimulationState{basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

instance Normalizable Variable where
  norm g v = norm g [v]


normUsingMap :: Set.Set Variable -> Word ->  Bisimulation Norm
normUsingMap _ [] = return (Normed 0)
normUsingMap visitedVars w@(v:vs) = do 
  (x,y) <- computeNorm visitedVars w v Set.empty
  deleteFromMap y
  return x


deleteFromMap :: Set.Set Variable -> Bisimulation ()
deleteFromMap vars = do
  map <- gets normMap
  let newMap = Set.foldr Map.delete map vars
  updateNormMap newMap


-- Calculates the norm using the NormMap
-- Receive a grammar, a word, a map and an array of "Variables" (variables already visited)
-- Returns a tuple with the values of the norm, the variables already visited and the NormMap
computeNorm :: Set.Set Variable -> Word-> Variable -> Set.Set Variable -> Bisimulation (Norm, Set.Set Variable)
computeNorm _ [] _ vars = return (Normed 0, vars)
computeNorm visitedVars w@(v : vs) parent varsToRecheck = do
  -- troco a ordem e ponho o visitedVars como parametro
  map <- gets normMap
  case Map.lookup v map of
    Just nVar -> do
      (nTail, vars) <- computeNorm visitedVars vs parent varsToRecheck
      return (nTail + nVar, vars)
    Nothing -> do
      if v `elem` visitedVars
        then do
          return (Unnormed, Set.insert parent varsToRecheck)
        else do
          (Grammar _ prods) <- gets grammar
          wordNorm visitedVars prods w parent varsToRecheck

wordNorm :: Set.Set Variable -> Productions -> Word -> Variable -> Set.Set Variable -> Bisimulation (Norm, Set.Set Variable)
wordNorm visitedVars prods (v : vs) parent varsToRecheck = do
  map <- gets normMap
  let newVisitedVars = Set.insert v visitedVars
      trans = transitions v prods
      toVisit = Map.elems trans -- ver se da para fazer sem listas/ noa consegui
  if [] `elem` toVisit
    then do
      let nVar = Normed 1
      updateNormMap (Map.insert v nVar map)
      (nTail,vars) <- computeNorm newVisitedVars vs parent varsToRecheck
      return (nVar + nTail, vars)
    else do
      (nVar,vars'') <-
        foldM
          ( \(acc,vars) w -> do
              (a,vars') <- computeNorm newVisitedVars w v vars
              case a of
                Unnormed -> do 
                  if (head w) == v 
                    then
                      return (min acc a,vars')
                    else do 
                      return (min acc a, Set.insert v vars')
                _ -> do
                  return (min acc a,vars')
          )
          (Unnormed, varsToRecheck)
          toVisit
      map <- gets normMap
      let nVar' = nVar + Normed 1
      updateNormMap (Map.insert v nVar' map)
      (nTail,vars''' )<- computeNorm newVisitedVars vs parent vars''
      return (nVar' + nTail,vars''')

-- Auxiliary function of normUsingMap
-- normUsingMapAux ::  [Word] -> NormState Norm
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
    initState = BisimulationState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Calculates the seminorm using the NormMap
-- Receive a grammar, a word, a map and an array of "Variables" (variables already visited)
-- Returns a tuple with the values of the norm, the variables already visited and the NormMap
seminormUsingMap :: Set.Set Variable -> Word -> Bisimulation Int
seminormUsingMap _ [] = return 0
seminormUsingMap visitedVars (x : xs) = do
  result <- normUsingMap visitedVars [x]
  case result of
    Unnormed -> return 0
    Normed y -> do
      result' <- seminormUsingMap visitedVars xs
      return (y + result')

---------------------------------------------------------------------------------------------------------------------

-- 4 Calculates the k-th term in the canonical seminorm-reducing sequence of a word
-- requires 0 <= i <= seminorm g w
yk :: Grammar -> Word -> Int -> Word
yk g w i = evalState (ykUsingMap Set.empty i w) initState
  where
    initState = BisimulationState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Calculates the  k-th term using the NormMap
-- Receive a grammar, a word, the k, a map and an array of "Variables" (variables already visited)
-- Returns a word
ykUsingMap :: Set.Set Variable -> Int -> Word -> Bisimulation Word
ykUsingMap _ 0 w = return w
ykUsingMap visitedVars i (x : xs) = do
  result <- normUsingMap visitedVars [x]
  case result of
    Normed n
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
    initState = BisimulationState {basis = Map.empty, visitedPairs = Set.empty, normMap = Map.empty, grammar = g, log = []}

-- Auxiliary function of valuation
valuationUsingMap :: Set.Set Variable -> [Word] -> Bisimulation Int
valuationUsingMap visitedVars = foldM (\acc w -> max acc <$> seminormUsingMap visitedVars w) 0

-----------------------------------------------------------------------------------------------------------------------------

reducesNorm :: Int -> Int -> Bool
reducesNorm x y = x == (y + 1)

getNormFromMap :: Variable -> NormMap -> Int
getNormFromMap x mp = case Map.lookup x mp of
  Just val -> case val of
    Unnormed -> -1
    Normed x -> x
  Nothing -> undefined

getNormWordFromMap :: Word -> NormMap -> Int
getNormWordFromMap [] _ = 0
getNormWordFromMap [x] mp = getNormFromMap x mp
getNormWordFromMap (x : xs) mp = getNormFromMap x mp + getNormWordFromMap xs mp
