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
  , DataOrCont(..)
  , DomOrRng(..)
  , FstOrSnd(..)
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
import           Parse.Unparser                 ( )
import qualified Data.Map.Strict as Map
import           Data.List                      ( intercalate )
-- Word is (re)defined in module Equivalence.Grammar
import           Prelude                 hiding ( Word )
import qualified Syntax.Type     as T
import qualified Syntax.Kind     as K           (PreKind(..), Kind(..), Multiplicity)

-- Terminal symbols are called labels
data Label = FatTerm String
           | Message T.Polarity     DataOrCont 
           | Arrow   DomOrRng
           | LinArrow 
           | Checkmark K.PreKind T.View
           | Almanac K.PreKind        T.View     Variable -- All this nesting may have an impact on performance...
           | Pair    FstOrSnd
           | Forall  K.Kind 
           | Var     String
  deriving (Eq, Ord)

data DataOrCont = Data   | Continuation deriving (Eq, Ord)
data DomOrRng   = Domain | Range        deriving (Eq, Ord)
data FstOrSnd   = Fst    | Snd          deriving (Eq, Ord)

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
instance Show Label where 
  show (FatTerm s) = s 
  show (Message p dc) = show p ++ show dc  
  show (Arrow dr) = "->"++ show dr  
  show LinArrow = "1->"
  show (Pair fs) = "," ++ show fs  
  show (Forall k) = "∀" ++ show k++show (getSpan k)
  show (Var a) = a
  show (Almanac K.Top     T.Internal l) = "{}" ++ show l 
  show (Almanac K.Top     T.External l) = "<>" ++ show l 
  show (Almanac K.Session T.Internal l) = "+"  ++ show l 
  show (Almanac K.Session T.External l) = "&"  ++ show l
  show (Checkmark K.Top     T.Internal) = "{}✓" 
  show (Checkmark K.Top     T.External) = "<>✓" 
  show (Checkmark K.Session T.Internal) = "+✓"  
  show (Checkmark K.Session T.External) = "&✓" 
  
instance Show DomOrRng where 
  show Domain = "d"
  show Range  = "r"

instance Show DataOrCont where 
  show Data         = "d"
  show Continuation = "c"

instance Show FstOrSnd where 
  show Fst = "fst"
  show Snd = "snd"

instance Show Grammar where
  show (Grammar xss p) =
    "start words: (" ++ intercalate ", " (map showWord xss) ++
    ")\nproductions: " ++ showProductions p

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
