{- |
Module      :  Grammar
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Context-free grammars of a certain kind:

- Non-terminal symbols are type variables (TVar)

- Terminal symbols are called labels (Label) for they come from the
labelled transition system on types

- Right-hand sides in productions are composed of (exactly) one label,
followed by a (possibly empty) sequence of type variables

- For each non-terminal symbol there is exactly one production.

This allows representing a grammar by a map from type-variables to a
map from labels to lists of type variables.

-}

module Equivalence.Grammar
( Label(..)
, TVar -- aka non-terminal
, Transitions
, Productions
, Grammar(..)
, transitions
, insertProduction
, trans
, pathToSkip
, throughPath
--, reachable
--, backwards
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Syntax.Types
import           Syntax.Bind
import           Data.List (union, delete)

-- Terminal symbols are called labels
data Label =
  ChoiceLabel Polarity PVar |
  MessageLabel Polarity BasicType |
  VarLabel TVar
  deriving (Eq, Ord)

-- Non-terminal symbols are type variables TVar

-- The transitions from a given label
type Transitions = Map.Map Label [TVar]

-- The productions of a grammar
type Productions = Map.Map TVar Transitions

-- The grammar, we have one initial non-terminal for each type that we
-- convert together
data Grammar = Grammar [TVar] Productions

-- Operations on grammars

-- The transitions from a word, as opposed to the transitions from a non-terminal
transitions :: Productions -> [TVar] -> Transitions
transitions _ []     = Map.empty
transitions p (x:xs) = Map.map (++ xs) (p Map.! x)

-- Add a production from a non-terminal; the productions may already contain transitions for the given nonterminal (hence the insertWith and union)
insertProduction :: Productions -> TVar -> Label -> [TVar] -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- Determine transitions from a word
trans :: Productions -> [TVar] -> [[TVar]]
trans p xs = Map.elems (transitions p xs)

-- only applicable on normed variables
pathToSkip :: Productions -> TVar -> [Label]
pathToSkip p x = fst . head $ filter ( null . snd ) ps
  where ps = pathToSkip' p ( Map.assocs $ (Map.mapKeys (\k -> [k]) (transitions p [x])) )

pathToSkip' :: Productions -> [([Label],[TVar])] -> [([Label],[TVar])]
pathToSkip' p ps
  | any (null . snd) ps = ps
  | otherwise           = pathToSkip' p ps'
  where ps' = foldr (\(ls,xs) ts -> union
                    (map (\(l,ys) -> (ls++[l], ys)) $ Map.assocs $ transitions p xs)
                    ts ) [] ps

throughPath :: Productions -> [Label] -> [TVar] -> Maybe [TVar]
throughPath p (l:ls) xs
  | not (Map.member l ts) = Nothing
  | otherwise = throughPath p ls xs'
  where ts  = (transitions p xs)
        xs' = ts Map.! l
throughPath p _ xs = Just xs

-- Showing a grammar

instance Show Label where
  show (ChoiceLabel v l) = show v ++ show l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

instance Show Grammar where
  show (Grammar xs p) =
    "start symbols: " ++ concat xs ++
    "\nproductions: " ++ showProductions p

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""

showTransitions :: TVar -> Transitions -> String -> String
showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

showTransition :: TVar -> Label -> [TVar] -> String -> String
showTransition x l xs s = s ++ "\n" ++ x ++ " -> " ++ show l ++ " " ++ concat xs
