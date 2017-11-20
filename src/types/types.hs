{-
 -
 -}

module Types
( BasicType(..)
, Type(..)
) where

-- BASIC TYPES

data BasicType =
  IntType |
  CharType |
  BoolType |
  UnitType
  deriving (Eq, Show)

-- TYPES

data Type =
  Skip |
  Semi Type Type |
  Out BasicType |
  In BasicType |
  Basic BasicType |
  UnFun Type Type |
  LinFun Type Type |
  Pair Type Type |
  Rec String Type |
  Var String |
  Forall String Type
  deriving (Show)
-- TODO: internal and external choice, datatypes
