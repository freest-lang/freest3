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

module Validation.Norm
( prune
, pruneWord
, normed
, sameNorm
, norm
, allNormed
, normedWord
) where

import           Data.List (union)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Validation.Grammar

-- Normalisation

prune :: Productions -> Productions
prune p = Map.map (Map.map (pruneWord p)) p

pruneWord :: Productions -> [TypeVar] -> [TypeVar]
pruneWord p = foldr (\x ys -> if normed p x then x:ys else [x]) []
-- pruneWord _ [] = []
-- pruneWord p (x:xs)
--   | normed p x = x : pruneWord p xs
--   | otherwise  = [x]

normed :: Productions -> TypeVar -> Bool
normed p x = normedWord p Set.empty [x]

type Visited = Set.Set TypeVar

normedWord :: Productions -> Visited -> [TypeVar] -> Bool
normedWord _ _ []     = True
normedWord p v (x:xs) =
  x `Set.notMember` v &&
  any (normedWord p v') (Map.elems (transitions p (x:xs)))
  where v' = if any (x `elem`) (Map.elems (transitions p [x])) then Set.insert x v else v

norm :: Productions -> [TypeVar] -> Int
norm p xs = normList p [xs]

normList :: Productions -> [[TypeVar]] -> Int
normList p xss
  | [] `elem` m = 0
  | otherwise = 1 + normList p (foldr union [] m)
  where m = map (trans p) xss

sameNorm :: Productions -> [TypeVar] -> [TypeVar] -> Bool
sameNorm p xs ys =
  (not normedXs && not normedYs) ||
  (normedXs && normedYs && norm p xs == norm p ys )
  where normedXs = normedWord p Set.empty xs
        normedYs = normedWord p Set.empty ys

-- Identify the existence of unnormed symbols

allNormed :: Productions -> Bool
allNormed p = and $ map (normed p) (Map.keys p)
