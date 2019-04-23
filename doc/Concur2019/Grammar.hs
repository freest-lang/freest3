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
import qualified Data.Set as Set
import           Data.List (union, delete)
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

-- The transitions from a word, as opposed to the transitions from a non-terminal
transitions :: Productions -> [TypeVar] -> Transitions
transitions _ []     = Map.empty
transitions p (x:xs) = Map.map (++ xs) (Map.findWithDefault Map.empty x p)
-- transitions p (x:xs) = Map.map (++ xs) (p Map.! x)

-- Add a production from a non-terminal; the productions may already contain transitions for the given nonterminal (hence the insertWith and union)
insertProduction :: Productions -> TypeVar -> Label -> [TypeVar] -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- Determine transitions from a word
trans :: Productions -> [TypeVar] -> [[TypeVar]]
trans p xs = Map.elems (transitions p xs)

-- only applicable to normed variables
pathToSkip :: Productions -> TypeVar -> [Label]
pathToSkip p x = fst . head $ filter (null . snd) ps
  where ps = pathToSkip' p (Map.assocs $ (Map.mapKeys (:[]) (transitions p [x])))

pathToSkip' :: Productions -> [([Label],[TypeVar])] -> [([Label],[TypeVar])]
pathToSkip' p ps
  | any (null . snd) ps = ps
  | otherwise           = pathToSkip' p ps'
  where ps' = foldr (\(ls,xs) ts -> union
                    (map (\(l,ys) -> (ls++[l], ys)) $ Map.assocs $ transitions p xs)
                    ts ) [] ps

throughPath :: Productions -> [Label] -> [TypeVar] -> Maybe [TypeVar]
throughPath p (l:ls) xs
  | not (Map.member l ts) = Nothing
  | otherwise = throughPath p ls xs'
  where ts  = (transitions p xs)
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
