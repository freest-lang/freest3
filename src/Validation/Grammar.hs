{- |
Module      :  Grammar
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Context-free grammars of a certain kind:

- Non-terminal symbols are type variables (TypeVar)

- Terminal symbols are called labels (Label) for they come from the
labelled transition system on types

- Right-hand sides in productions are composed of (exactly) one label,
followed by a (possibly empty) sequence of type variables

- For each non-terminal symbol there is exactly one production.

This allows representing a grammar by a map from type-variables to a
map from labels to lists of type variables.

-}

module Validation.Grammar
( Label(..)
, TypeVar -- aka non-terminal
, Transitions
, Productions
, Grammar(..)
, transitions
, insertProduction
, trans
--, reachable
--, backwards
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Syntax.Types
import           Data.List (union,delete)

-- Terminal symbols are called labels
data Label =
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

-- Non-terminal symbols are type variables TypeVar

-- The transitions from a given label
type Transitions = Map.Map Label [TypeVar]

-- The productions of a grammar
type Productions = Map.Map TypeVar Transitions

-- The grammar, we have one initial non-terminal for each type that we
-- convert together
data Grammar = Grammar [TypeVar] Productions

-- Operations on grammars

-- The transitions from a word, as opposed to the transitions from a non-terminal
transitions :: Productions -> [TypeVar] -> Transitions
transitions _ []     = Map.empty
transitions p (x:xs) = Map.map (++ xs) (p Map.! x)

-- Add a production from a non-terminal; the productions may already contain transitions for the given nonterminal (hence the insertWith and union)
insertProduction :: Productions -> TypeVar -> Label -> [TypeVar] -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- Determine transitions from a word
trans :: Productions -> [TypeVar] -> [[TypeVar]]
trans p xs = Map.elems (transitions p xs)

-- reachable :: Productions -> Set.Set TypeVar -> Set.Set TypeVar
-- reachable p xs
--   | xs == ts  = xs
--   | otherwise = reachable p ts
--   where ys = foldr union [] $ trans p (Set.toList xs)
--         ts = Set.union xs (Set.fromList ys)

-- backwards :: Productions -> [TypeVar] -> TypeVar
-- backwards p xs
--   | not (null k) = head k
--   | otherwise    = backwards p (Map.keys ps)
--   where ps = Map.filter (\y -> or (map (`elem` (foldr union [] (Map.elems y))) xs)) p
--         f  = Map.filter (\y ->  Map.member (MessageLabel In UnitType) y) ps
--         k  = Map.keys f

-- Showing a grammar

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

instance Show Grammar where
  show (Grammar xs p) =
    "start symbols: " ++ concat xs ++
    "\nproductions: " ++ showProductions p

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""

showTransitions :: TypeVar -> Transitions -> String -> String
showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

showTransition :: TypeVar -> Label -> [TypeVar] -> String -> String
showTransition x l xs s = s ++ "\n" ++ x ++ " -> " ++ show l ++ " " ++ concat xs
