{- |
Module      :  Syntax.Base
Description :  The Base module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the basic structures (classes and datatypes) such as Positions and
Multiplicity, that will be used the remaining Compiler.
-}

module Syntax.Base
  ( Default(..)
  , Pos
  , Multiplicity(..)
  , Bind(..)
  , Variable(..)
  , intern
  , extern
  , mkVar
  , mkNewVar
  , Span(..)
  , defaultSpan
  , Located(..)
  , isWild
) where

import           Data.Char ( isDigit )

-- Default value for the various syntactic categories

class Default t where
  omission :: Span -> t

-- Position: line and column

type Pos = (Int, Int)

-- Location information for the various syntactic categories

data Span = Span
  { startPos   :: Pos
  , endPos     :: Pos
  , moduleName :: FilePath
  } deriving (Eq, Ord)

defaultSpan :: Span
defaultSpan = Span (0, 0) (0, 0) "<default>"

class Located t where
  getSpan :: t -> Span

-- Multiplicity for types and expressions

data Multiplicity = Un | Lin deriving Eq

-- Variables for types and programs

data Variable = Variable Span String

instance Eq Variable where
  x == y = intern x == intern y
  
instance Ord Variable where
  x <= y = intern x <= intern y

instance Located Variable where
  getSpan (Variable s _) = s

instance Default Variable where
  omission s = mkVar s "<default>"

-- The internal representation of a variable
intern :: Variable -> String
intern (Variable _ x) = x

-- The program-level representation of a variable
extern :: Variable -> String
extern (Variable _ x) = showVar x

showVar :: String -> String
showVar = dropWhile (\c -> isDigit c || c == '#')

-- Making a variable from a string
mkVar :: Span -> String -> Variable
mkVar = Variable

-- Making a new variable from a given variable. The new variable is unique if
-- the integer is
mkNewVar :: Int -> Variable -> Variable
mkNewVar next (Variable s str) = Variable s (show next ++ '#' : showVar str)

isWild :: Variable -> Bool
isWild (Variable _ x) = x == "_"

-- Bind for (λ x:t -> e), (∀ a:k . t) or (Λ a:k => e)

data Bind a b = Bind {bSpan :: Span, var :: Variable, binder :: a, body :: b}
