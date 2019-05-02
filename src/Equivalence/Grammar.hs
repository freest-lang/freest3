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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Equivalence.Grammar
( Label(..)
, Transitions
, Productions
, Grammar(..)
, transitions
, insertProduction
, trans
, pathToSkip
, throughPath
) where

import           Syntax.Types
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Syntax.Show
import qualified Data.Map.Strict as Map
import           Data.List (union)
import           Data.List (intersperse)

-- Terminal symbols are called labels
data Label =
  ChoiceLabel Polarity ProgVar |
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

class TransitionsFrom t where
  transitions :: t -> Productions -> Transitions

-- The transitions from a non-terminal
instance TransitionsFrom TypeVar where
  transitions = Map.findWithDefault Map.empty
  
-- The transitions from a word
instance TransitionsFrom [TypeVar] where
  transitions []     _ = Map.empty
  transitions (x:xs) p = Map.map (++ xs) (transitions x p)

-- Add a production from a non-terminal; the productions may already
-- contain transitions for the given nonterminal (hence the insertWith
-- and union)
insertProduction :: Productions -> TypeVar -> Label -> [TypeVar] -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- Determine transitions from a word
trans :: Productions -> [TypeVar] -> [[TypeVar]]
trans p xs = Map.elems (transitions xs p)

-- only applicable to normed variables
pathToSkip :: Productions -> TypeVar -> [Label]
pathToSkip p x = fst . head $ filter (null . snd) ps
  where ps = pathToSkip' p (Map.assocs $ (Map.mapKeys (:[]) (transitions x p)))

pathToSkip' :: Productions -> [([Label],[TypeVar])] -> [([Label],[TypeVar])]
pathToSkip' p ps
  | any (null . snd) ps = ps
  | otherwise           = pathToSkip' p ps'
  where ps' = foldr (\(ls,xs) ts -> union
                    (map (\(l,ys) -> (ls++[l], ys)) $ Map.assocs $ transitions xs p)
                    ts ) [] ps

throughPath :: Productions -> [Label] -> [TypeVar] -> Maybe [TypeVar]
throughPath p (l:ls) xs
  | not (Map.member l ts) = Nothing
  | otherwise = throughPath p ls xs'
  where ts  = transitions xs p
        xs' = ts Map.! l
throughPath p _ xs = Just xs

-- Showing a grammar

instance Show Label where
  show (ChoiceLabel v l)  = showChoiceView v ++ intern l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l)       = intern l

instance Show Grammar where
  show (Grammar xs p) =
    "start symbols: " ++ concat (map intern xs) ++
    "\nproductions: " ++ showProductions p

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""

showTransitions :: TypeVar -> Transitions -> String -> String
showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

showTransition :: TypeVar -> Label -> [TypeVar] -> String -> String
showTransition x l xs s = s ++ "\n" ++ intern x ++ " -> " ++ show l ++ " " ++ concat (intersperse " " (map intern xs))
