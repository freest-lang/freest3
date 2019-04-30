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
( normed
, norm
, allNormed
, sameNorm 
) where

import           Syntax.TypeVariables
import           Equivalence.Grammar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe

normed :: Productions -> TypeVar -> Bool
normed p x = isJust $ nor p [x]

norm :: Productions -> [TypeVar] -> Int
norm p xs = fromJust $ nor p xs

sameNorm :: Productions -> [TypeVar] -> [TypeVar] -> Bool
sameNorm p xs ys = nor p xs == nor p ys

-- Identify the existence of unnormed symbols
allNormed :: Productions -> Bool
allNormed p = all (normed p) (Map.keys p)

type Visited = Set.Set TypeVar

nor :: Productions -> [TypeVar] -> Maybe Int
nor p = norm' Set.empty
  where
  norm' :: Visited -> [TypeVar] -> Maybe Int
  norm' _ [] = Just 0
  norm' v xs
    | (head xs) `Set.member` v &&
        not (Map.null (Map.filter (not . null) (transitions (head xs) p))) = Nothing
    | otherwise                = fmap (+1) (Map.foldr min' Nothing (norms v xs))
  norms :: Visited -> [TypeVar] ->  Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm' (Set.insert (head xs) v)) (transitions xs p)
  min' :: Maybe Int -> Maybe Int -> Maybe Int
  min' Nothing  m        = m
  min' m        Nothing  = m
  min' (Just n) (Just k) = Just (min n k)
