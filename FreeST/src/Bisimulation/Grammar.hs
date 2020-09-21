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

{-# LANGUAGE FlexibleInstances #-}

module Bisimulation.Grammar
  ( Label
  , Transitions
  , Productions
  , Grammar(..)
  , Word
  , transitions
  , insertProduction
--, trans
  )
where

import           Syntax.TypeVariables
import           Syntax.Base
import           Syntax.Show
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intersperse )
import           Prelude                 hiding ( Word ) -- Word is (re)defined in module Equivalence.Grammar

-- Terminal symbols are called labels
type Label = String

-- Non-terminal symbols are type variables TypeVar
-- Words are strings of non-terminal symbols
type Word = [TypeVar]

-- The transitions from a given non-terminal
type Transitions = Map.Map Label Word

-- The productions of a grammar
type Productions = Map.Map TypeVar Transitions

-- The grammar, we have one initial non-terminal for each type that we
-- convert together
data Grammar = Grammar [Word] Productions

-- Operations on grammars

class TransitionsFrom t where
  transitions :: t -> Productions -> Transitions

-- The transitions from a non-terminal
instance TransitionsFrom TypeVar where
  transitions = Map.findWithDefault Map.empty

-- The transitions from a word
instance TransitionsFrom Word where
  transitions []       _ = Map.empty
  transitions (x : xs) p = Map.map (++ xs) (transitions x p)

-- Add a production from a non-terminal; the productions may already
-- contain transitions for the given nonterminal (hence the insertWith
-- and union)
insertProduction :: Productions -> TypeVar -> Label -> Word -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- The transitions from a word
-- trans :: Productions -> Word -> [Word]
-- trans p xs = Map.elems (transitions xs p)

-- Showing a grammar

instance Show Grammar where
  show (Grammar xss p) =
    "start words: "
      ++ concat (intersperse ", " (map showWord xss))
      ++ "\nproductions: "
      ++ showProductions p

showWord :: Word -> String
showWord = foldr (\x acc -> show x ++ acc) ""

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""
 where
  showTransitions :: TypeVar -> Transitions -> String -> String
  showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

  showTransition :: TypeVar -> Label -> Word -> String -> String
  showTransition x l xs s =
    s ++ "\n" ++ intern x ++ " -> " ++ l ++ " " ++ concat
      (intersperse " " (map intern xs))
