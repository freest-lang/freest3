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
, sameNorm -- TODO: remove and replace by ==
) where

import           Syntax.TypeVariables
import           Equivalence.Grammar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Data.List

normed :: Productions -> TypeVar -> Bool
normed p x = isJust $ maybeNorm p [x]

norm :: Productions -> [TypeVar] -> Int
norm p xs = fromJust $ maybeNorm p xs

sameNorm :: Productions -> [TypeVar] -> [TypeVar] -> Bool
sameNorm p xs ys = maybeNorm p xs == maybeNorm p ys

-- Identify the existence of unnormed symbols
allNormed :: Productions -> Bool
allNormed p = all (normed p) (Map.keys p)

type Visited = Set.Set [TypeVar]

maybeNorm :: Productions -> [TypeVar] -> Maybe Int
maybeNorm p = norm Set.empty
  where
  norm :: Visited -> [TypeVar] -> Maybe Int
  norm _ [] = Just 0
  norm v xs
    | not $ Set.null $ Set.filter (flip isSubsequenceOf xs) v = Nothing
    | otherwise = fmap (+1) (Map.foldr (compose min) Nothing (norms v xs))
  norms :: Visited -> [TypeVar] ->  Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm (Set.insert xs v)) (transitions xs p)

compose :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
compose _ Nothing  m        = m
compose _ m        Nothing  = m
compose f (Just x) (Just y) = Just (f x y)
  
{-
nor :: Productions -> [TypeVar] -> Maybe Int
nor p xs = norm' 0 xs
  where
  norm' :: Visited -> [TypeVar] -> Maybe Int
  norm' n [] = Just (n + 1)
  norm' n xs
    | n > m+1     = Nothing
    | otherwise = fmap (+1) (Map.foldr min' Nothing (norms v xs))
  norms :: Visited -> [TypeVar] ->  Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm' n+1) (transitions xs p)
  min' :: Maybe Int -> Maybe Int -> Maybe Int
  min' Nothing  m        = m
  min' m        Nothing  = m
  min' (Just n) (Just k) = Just (min n k)
  m = symbols Set.empty (head xs)
-}

