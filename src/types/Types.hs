{-
 -
 -}

module Types
( BasicType(..)
, Type(..)
, Id
) where

import qualified Data.Map.Strict as Map

-- BASIC TYPES

data BasicType =
  IntType |
  CharType |
  BoolType |
  UnitType
  deriving (Eq, Show)

-- TYPES

data Type =
  Basic BasicType |
  Skip |
  Semi Type Type |
  Out BasicType |
  In BasicType |
  UnFun Type Type |
  LinFun Type Type |
  Pair Type Type |
  ExternalChoice TypeMap |
  InternalChoice TypeMap |
  Datatype TypeMap |
  Rec String Type |
  Forall String Type |
  Var String
  deriving (Eq, Show) -- This Eq must be redefined

type Id = String

type TypeMap = Map.Map Id Type
