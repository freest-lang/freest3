{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
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

-- type Pos = AlexPosn -- TODO: Make Pos a newtype so that we may redefine Show
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
