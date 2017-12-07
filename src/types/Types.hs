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
  deriving (Eq)

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
  deriving (Eq) -- This Eq must be redefined

type Id = String

type TypeMap = Map.Map Id Type

-- TODO:
instance Show BasicType where
   show IntType = "Int"
   show CharType = "Char"
   show BoolType = "Bool"
   show UnitType = "()"


instance Show Type where
  show (Basic x) = show x
  show Skip = "Skip"
  show (Semi x y) = "(" ++ show x ++ ";" ++ show y ++ ")"
  show (Out x) = "(" ++ "!" ++ show x ++ ")"
  show (In x) = "(" ++ "?" ++ show x ++ ")"
  show x = show x
