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
  , defaultPos
  , negPos
  , Bind(..)
  , Variable(..)
  , intern
  , mkVar
  , mkNewVar
  , Span(..)
  , defaultSpan
  , Located(..)
  , negSpan
  , isWild
) where

-- Default for the various syntactic categories

class Default t where
  omission :: Span -> t

-- Position

type Pos = (Int, Int)

-- class Position t where
--   pos :: t -> Pos  

defaultPos :: Pos
defaultPos = (0, 0)

negPos :: Pos -> Pos
negPos (i, j) = (negate i, negate j)

-- Span

class Located t where
  getSpan :: t -> Span
  setSpan :: Span -> t ->  t
  
  setSrc :: String -> t -> t
  setSrc src x = setSpan s{source = src} x
    where s = getSpan x

  -- copies start and end positions from the first into the second, keeps all else
  changePos :: Span -> t -> t
  changePos s y = setSpan sy{startPos= startPos s, endPos= endPos s} y
    where sy = getSpan y

data Span = Span
  { startPos     :: Pos
  , endPos       :: Pos
  , source       :: String
  , defModule    :: FilePath
  } deriving (Eq, Ord)

defaultSpan :: Span
defaultSpan = Span defaultPos defaultPos "" ""

negSpan :: Span -> Span
negSpan s = s {startPos = negPos (startPos s), endPos = negPos (endPos s)}

-- Multiplicity for types and expressions

data Multiplicity = Un | Lin deriving Eq

-- Type and program variable
data Variable = Variable Span String

instance Eq Variable where
  (Variable _ x) == (Variable _ y) = x == y
  
instance Ord Variable where
  (Variable _ x) <= (Variable _ y) = x <= y

-- instance Position Variable where
--   pos (Variable p _) = startPos p
  
instance Located Variable where
  getSpan (Variable p _) = p
  setSpan s (Variable _ v) = Variable s v

instance Default Variable where
  omission p = mkVar p "omission"

-- The string, internal representation of a variable
intern :: Variable -> String
intern (Variable _ x) = x

-- Making a variable from a string, type or program
mkVar :: Span -> String -> Variable
mkVar s name = Variable s{source=name} name

isWild :: Variable -> Bool
isWild (Variable _ x) = x == "_"

-- Making a new variable from a given variable. The variable is
-- unique up to the point where the integer is
mkNewVar :: Int -> Variable -> Variable
mkNewVar next v = Variable (getSpan v) (show next)

-- Bind: (λ x:t -> e), (∀ a:k . t) or (Λ a:k => e) 
data Bind a b = Bind {bSpan :: Span, var :: Variable, binder :: a, body :: b}
