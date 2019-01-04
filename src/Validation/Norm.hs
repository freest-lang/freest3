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
( normalise
, normed
, norm
) where

import           Data.List (union)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Validation.Grammar
import           Syntax.Types

-- Normalisation

normalise :: Productions -> Productions
normalise g = Map.map (Map.map (normaliseWord g)) g

normaliseWord :: Productions -> [TypeVar] -> [TypeVar]
normaliseWord _ []     = []
normaliseWord g (x:xs)
  | normed g x = x : normaliseWord g xs
  | otherwise  = [x]

normed :: Productions -> TypeVar -> Bool
normed g x = normedWord g Set.empty [x]

normedWord :: Productions -> Set.Set TypeVar -> [TypeVar] -> Bool
normedWord _ _ []     = True
normedWord g v (x:xs) =
  not (x `Set.member` v) &&
  any id (map (normedWord g (Set.insert x v)) (Map.elems (transitions g (x:xs))))

norm :: Productions -> [TypeVar] -> Int
norm _ []   = 0 -- TODO: redundant; remove
norm g xs = normList g [xs]

normList :: Productions -> [[TypeVar]] -> Int       -- TODO: use Set [TypeVar] rather than [[TypeVar]]
normList g xs
  | [] `elem` m = 0
  | otherwise = 1 + (normList g (foldr union [] m))
  where m = map (trans g) xs

trans :: Productions -> [TypeVar] -> [[TypeVar]]
trans g xs = Map.elems (transitions g xs)
