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
, Pos(..) 
, Position(..)
, Multiplicity(..)
, defaultPos
, negPos
, Bind(..)
, Variable(..)
, intern
, mkVar
, mkNewVar
) where

-- Default for the various syntactic categories

class Default t where
  omission :: Pos -> t

-- Position

data Pos = Pos Int Int deriving (Eq, Ord)

class Position t where
  pos :: t -> Pos  

defaultPos :: Pos
defaultPos = Pos 0 0

negPos :: Pos -> Pos
negPos (Pos i j) = Pos (negate i) (negate j)

-- Multiplicity for types and expressions

data Multiplicity = Un | Lin deriving Eq

-- Type and program variable
data Variable = Variable Pos String

instance Eq Variable where
  (Variable _ x) == (Variable _ y) = x == y
  
instance Ord Variable where
  (Variable _ x) <= (Variable _ y) = x <= y

instance Position Variable where
  pos (Variable p _) = p

instance Default Variable where
  omission p = mkVar p "omission"

-- The string, internal representation of a variable
intern :: Variable -> String
intern (Variable _ x) = x

-- Making a variable from a string, type or program
mkVar :: Pos -> String -> Variable
mkVar = Variable
-- Making a new variable from a given variable. The variable is
-- unique up to the point where the integer is
mkNewVar :: Int -> Variable -> Variable
mkNewVar next (Variable p str) = Variable p (show next ++ '#' : str)

-- Bind: (λ x:t -> e), (∀ a:k . t) or (Λ a:k => e) 
data Bind a b = Bind {posn :: Pos, var :: Variable, binder :: a, body :: b}