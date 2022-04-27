{- |
Module      :  Equivalence.Norm
Description :  Normed and unnormed words.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

The concept of normed words. Words are sequences of non-terminal symbols. Non
terminal symbols are variables.
-}

module Bisimulation.Norm
( normed
, norm
, allNormed
, equallyNormed
, prune
) where

import           Syntax.Base          (Variable)
import           Bisimulation.Grammar (Word, Productions, Label, transitions)
import           Data.Maybe           (isJust, fromJust)
import           Data.List            (isSubsequenceOf)
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Prelude              hiding (Word) -- redefined in module Bisimulation.Grammar

-- | xs is normed when xs -as-> ε for some non-empty sequence of nonterminal
-- symbols as.
normed :: Productions -> Variable -> Bool
normed p x = isJust $ maybeNorm p [x]

-- | When xs is normed, the minimal path of xs is the shortest as such that
-- xs -as-> ε. In this case, the norm of xs is the length of as.
norm :: Productions -> Word -> Int
norm p = fromJust . maybeNorm p

-- | xs and ys are equally normed when they are both normed or both unnormed.
equallyNormed :: Productions -> Word -> Word -> Bool
equallyNormed p xs ys = maybeNorm p xs == maybeNorm p ys

-- | Given a map of productions from non-terminal symbols x, are all sequences
-- [x] normed?
allNormed :: Productions -> Bool
allNormed p = all (normed p) (Map.keys p)

type Visited = Set.Set Word

-- | Nothing if xs is unnormed; Just n if n is xs is normed and n is the norm of
-- xs.
maybeNorm :: Productions -> Word -> Maybe Int
maybeNorm p = norm Set.empty
  where
  norm :: Visited -> Word -> Maybe Int
  norm _ [] = Just 0
  norm v xs
    | any (`isSubsequenceOf` xs) v = Nothing
    | otherwise = fmap (+1) (Map.foldr (unionMaybeWith min) Nothing (norms v xs))
  norms :: Visited -> Word -> Map.Map Label (Maybe Int)
  norms v xs = Map.map (norm (Set.insert xs v)) (transitions xs p)

-- | Union of two Maybe, cf., Agda.Utils.Maybe
unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith _ Nothing  m        = m
unionMaybeWith _ m        Nothing  = m
unionMaybeWith f (Just x) (Just y) = Just (f x y)

-- | Any unnormed word xs is bisimilar to its concatenation with any other word
-- ys. We use this fact to prune words ys from productions.
prune :: Productions -> Productions
prune p = Map.map (Map.map pruneWord) p
  where
    pruneWord :: Word -> Word
    pruneWord = foldr (\x ys -> x : if normed p x then ys else []) []

