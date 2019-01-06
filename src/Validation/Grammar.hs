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
, Transitions
, Productions
, Grammar(..)
, transitions
, insertProduction
) where

import qualified Data.Map.Strict as Map
import           Syntax.Types

-- The representation of a grammar

data Label =
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

-- The transitions from a given label
type Transitions = Map.Map Label [TypeVar]

-- The productions of a grammar
type Productions = Map.Map TypeVar Transitions

-- The grammar
data Grammar = Grammar {start :: TypeVar, productions :: Productions}

-- Operations on grammars

transitions :: Productions -> [TypeVar] -> Transitions
transitions _ []     = Map.empty
transitions p (x:xs) = Map.map (++ xs) (p Map.! x)

insertProduction :: Productions -> TypeVar -> Label -> [TypeVar] -> Productions
-- insertProduction p x l w = Map.insert x (Map.singleton l w) p
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- Showing grammars

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

instance Show Grammar where
  show g = "start: " ++ start g ++ showProductions (productions g)

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""

showTransitions :: TypeVar -> Transitions -> String -> String
showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

showTransition :: TypeVar -> Label -> [TypeVar] -> String -> String
showTransition x l xs s = s ++ "\n" ++ x ++ " -> " ++ show l ++ " " ++ concat xs

{-
showRHS :: [TypeVar] -> String
showRHS [] = "ε"
showRHS xs = concat xs
-}
