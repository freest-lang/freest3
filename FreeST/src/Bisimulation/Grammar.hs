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
  ( Label(..)
  , Transitions
  , Productions
  , Grammar(..)
  , Word
  , transitions
  , insertProduction
--, trans
  )
where

import           Syntax.Base
import qualified Syntax.Kind as K 
import qualified Syntax.Type as T 
import           Parse.Unparser                 ( )
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intercalate )
-- Word is (re)defined in module Equivalence.Grammar
import           Prelude                 hiding ( Word )

-- Terminal symbols are called labels
-- Terminal symbols are called labels
data Label = FatTerm String
           | ArrowD
           | ArrowR
           | Arrow1
           | End T.Polarity
           | Label T.Sort String
           | Labelled T.Sort 
           | MessageP T.Polarity
           | MessageC T.Polarity  
           | Pair1
           | Pair2
           | Forall String K.Kind 
           | Var String    
  deriving (Eq, Ord)

-- Non-terminal symbols are type variables Variable
-- Words are strings of non-terminal symbols
type Word = [Variable]

-- The transitions from a given non-terminal
type Transitions = Map.Map Label Word

-- The productions of a grammar
type Productions = Map.Map Variable Transitions

-- The grammar, we have one initial non-terminal for each type that we
-- convert together
data Grammar = Grammar [Word] Productions

-- Operations on grammars

class TransitionsFrom t where
  transitions :: t -> Productions -> Transitions

-- The transitions from a non-terminal
instance TransitionsFrom Variable where
  transitions = Map.findWithDefault Map.empty

-- The transitions from a word
instance TransitionsFrom Word where
  transitions []       _ = Map.empty
  transitions (x : xs) p = Map.map (++ xs) (transitions x p)

-- Add a production from a non-terminal; the productions may already
-- contain transitions for the given nonterminal (hence the insertWith
-- and union)
insertProduction :: Productions -> Variable -> Label -> Word -> Productions
insertProduction p x l w = Map.insertWith Map.union x (Map.singleton l w) p

-- The transitions from a word
-- trans :: Productions -> Word -> [Word]
-- trans p xs = Map.elems (transitions xs p)

-- Showing a grammar
instance Show Grammar where
  show (Grammar xss p) =
    "start words: (" ++ intercalate ", " (map showWord xss) ++
    ")\nproductions: " ++ showProductions p

instance Show Label where 
  show (FatTerm s) = s 
  show ArrowD = "->d"
  show ArrowR = "->r"
  show Arrow1 = "->1"
  show (End p) = show (T.End defaultSpan p)
  show (Label (T.Choice v) l) = show v ++ l
  show (Label s l) = show (Labelled s) ++ l
  show (Labelled T.Record) = "{}"
  show (Labelled T.Variant) = "⟨⟩"
  show (Labelled (T.Choice v)) = show v ++ "{}"
  show (MessageP p) = show p ++ "p"   
  show (MessageC p) = show p ++ "c"
  show Pair1 = "π1" 
  show Pair2 = "π2" 
  show (Forall a k) = "∀" ++ a ++ ":" ++ show k
  show (Var a) = a

showWord :: Word -> String
showWord = unwords . map intern

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showTransitions ""
  where
    showTransitions :: Variable -> Transitions -> String -> String
    showTransitions x m s = s ++ Map.foldrWithKey (showTransition x) "" m

    showTransition :: Variable -> Label -> Word -> String -> String
    showTransition x l xs s =
      s ++ "\n" ++ intern x ++ " -> " ++ show l ++ " " ++ showWord xs
