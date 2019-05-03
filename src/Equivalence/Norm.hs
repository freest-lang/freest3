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

normed :: Productions -> TypeVar -> Bool
normed p x = isJust $ nor p [x]
-- normed p x = normedWord p [x]

-- normedWord :: Productions -> [TypeVar] -> Bool
-- normedWord = normedW Set.empty
--   where
--   normedW :: Visited -> Productions -> [TypeVar] -> Bool
--   normedW _ _ []     = True
--   normedW v p (x:xs) =
--     x `Set.notMember` v &&
--    any (normedW v' p) (Map.elems (transitions (x:xs) p))
--    where v' = if any (x `elem`) (Map.elems (transitions x p)) then Set.insert x v else v

norm :: Productions -> [TypeVar] -> Int
norm p xs = fromJust $ nor p xs
-- norm p xs = normList p [xs]

-- normList :: Productions -> [[TypeVar]] -> Int
-- normList p xss
--   | [] `elem` m = 0
--   | otherwise = 1 + normList p (foldr List.union [] m)
--   where m = map (trans p) xss

sameNorm :: Productions -> [TypeVar] -> [TypeVar] -> Bool
sameNorm p xs ys = nor p xs == nor p ys
  -- not (normedWord p xs) || not (normedWord p ys) || norm p xs == norm p ys

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
