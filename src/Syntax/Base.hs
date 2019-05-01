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
, Multiplicity(..)
, Variable(..)
, Pos(..) 
, Position(..)
, defaultPos
) where

-- Defaults for the various syntactic categories

class Default t where
  omission :: Pos -> t

-- POSITIONS

data Pos = Pos Int Int

class Position t where
  position :: t -> Pos  

defaultPos :: Pos
defaultPos = Pos 0 0

-- Multiplicities for kinds, types, and expressions

data Multiplicity = Un | Lin deriving Eq

instance Ord Multiplicity where
  Un <= Lin = True
  _  <= _  = False

-- Variables, type and program

class Position t => Variable t where
  -- The string, internal representation of a variable
  intern :: t -> String
  -- Making a variable from a string, type or program
  mkVar :: Pos -> String -> t
  -- Making a new variable from a given variable. The variable is
  -- unique up to the point where the integer is
  mkNewVar :: Int -> t -> t
