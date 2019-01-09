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
, normed
, norm
) where

import           Data.List (union)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Validation.Grammar

-- Normalisation

prune :: Productions -> Productions
prune p = Map.map (Map.map (pruneWord p)) p

pruneWord :: Productions -> [TypeVar] -> [TypeVar]
pruneWord _ [] = []
pruneWord p (x:xs)
  | normed p x = x : pruneWord p xs
  | otherwise  = [x]

normed :: Productions -> TypeVar -> Bool
normed p x = normedWord p Set.empty [x]

normedWord :: Productions -> Set.Set TypeVar -> [TypeVar] -> Bool
normedWord _ _ []     = True
normedWord p v (x:xs) =
  x `Set.notMember` v &&
  or (map (normedWord p (Set.insert x v))
          (Map.elems (transitions p (x:xs))))

norm :: Productions -> [TypeVar] -> Int
norm p xs = normList p [xs]

normList :: Productions -> [[TypeVar]] -> Int
normList p xss
  | [] `elem` m = 0
  | otherwise = 1 + normList p (foldr union [] m)
  where m = map (trans p) xss

trans :: Productions -> [TypeVar] -> [[TypeVar]]
trans p xs = Map.elems (transitions p xs)
