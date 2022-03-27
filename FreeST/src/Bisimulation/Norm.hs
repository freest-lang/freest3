{- |
Module      :  Equivalence.Norm
Description :  The norm module 
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module is responsible for pruning unreachable symbols in unnormed sequences of symbols
-}

module Bisimulation.Norm
( normed
, norm
, allNormed
, sameNorm
) where

import          Syntax.Base (Variable)
import           Bisimulation.Grammar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Data.List

normed :: Productions -> Variable -> Bool
normed p x = isJust $ maybeNorm p [x]

norm :: Productions -> [Variable] -> Int
norm p xs = fromJust $ maybeNorm p xs

sameNorm :: Productions -> [Variable] -> [Variable] -> Bool
sameNorm p xs ys = maybeNorm p xs == maybeNorm p ys

-- Identify the existence of unnormed symbols
allNormed :: Productions -> Bool
allNormed p = all (normed p) (Map.keys p)

type Visited = Set.Set [Variable]

maybeNorm :: Productions -> [Variable] -> Maybe Int
maybeNorm p = norm Set.empty
  where
  norm :: Visited -> [Variable] -> Maybe Int
  norm _ [] = Just 0
  norm v xs
    | any (`isSubsequenceOf` xs) v = Nothing
    | otherwise = fmap (+1) (Map.foldr (compose min) Nothing (norms v xs))
  norms :: Visited -> [Variable] -> Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm (Set.insert xs v)) (transitions xs p)

compose :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
compose _ Nothing  m        = m
compose _ m        Nothing  = m
compose f (Just x) (Just y) = Just (f x y)
  
{-
nor :: Productions -> [Variable] -> Maybe Int
nor p xs = norm' 0 xs
  where
  norm' :: Visited -> [Variable] -> Maybe Int
  norm' n [] = Just (n + 1)
  norm' n xs
    | n > m+1     = Nothing
    | otherwise = fmap (+1) (Map.foldr min' Nothing (norms v xs))
  norms :: Visited -> [Variable] ->  Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm' n+1) (transitions xs p)
  min' :: Maybe Int -> Maybe Int -> Maybe Int
  min' Nothing  m        = m
  min' m        Nothing  = m
  min' (Just n) (Just k) = Just (min n k)
  m = symbols Set.empty (head xs)
-}

