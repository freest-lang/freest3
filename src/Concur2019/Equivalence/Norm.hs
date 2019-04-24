{- |
Module      :  Equivalence.Norm
Description :  The norm module 
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module is responsible for pruning unreachable symbols in unnormed sequences of symbols
-}

module Equivalence.Norm
( prune
, pruneWord
, normed
, sameNorm
, norm
, allNormed
) where

import           Syntax.TypeVariables
import           Equivalence.Grammar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

prune :: Productions -> Productions
prune p = Map.map (Map.map (pruneWord p)) p

pruneWord :: Productions -> [TypeVar] -> [TypeVar]
pruneWord p = foldr (\x ys -> if normed p x then x:ys else [x]) []

normed :: Productions -> TypeVar -> Bool
normed p x = normedWord p [x]

type Visited = Set.Set TypeVar

normedWord :: Productions -> [TypeVar] -> Bool
normedWord = normedW Set.empty
  where
  normedW :: Visited -> Productions -> [TypeVar] -> Bool
  normedW _ _ []     = True
  normedW v p (x:xs) =
    x `Set.notMember` v &&
   any (normedW v' p) (Map.elems (transitions p (x:xs)))
   where v' = if any (x `elem`) (Map.elems (transitions p [x])) then Set.insert x v else v

norm :: Productions -> [TypeVar] -> Int
norm p xs = normList p [xs]

normList :: Productions -> [[TypeVar]] -> Int
normList p xss
  | [] `elem` m = 0
  | otherwise = 1 + normList p (foldr List.union [] m)
  where m = map (trans p) xss

sameNorm :: Productions -> [TypeVar] -> [TypeVar] -> Bool
sameNorm p xs ys =
  not (normedWord p xs) || not (normedWord p ys) || norm p xs == norm p ys

-- Identify the existence of unnormed symbols
allNormed :: Productions -> Bool
allNormed p = all (normed p) (Map.keys p)
