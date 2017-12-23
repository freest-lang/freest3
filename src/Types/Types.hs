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

module Types.Types
( BasicType(..)
, Type(..)
, TypeMap(..)
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
  -- deriving (Eq,Show)


type Id = String

type TypeMap = Map.Map Id Type

-- TODO: Review
instance Show BasicType where
   show IntType = "Int"
   show CharType = "Char"
   show BoolType = "Bool"
   show UnitType = "()"


instance Show Type where
  show (Basic x) = show x
  show Skip = "skip"
  show (Semi x y) = "(" ++ show x ++ ";" ++ show y ++ ")"
  show (Out x) = "(" ++ "!" ++ show x ++ ")"
  show (In x) = "(" ++ "?" ++ show x ++ ")"
  show (UnFun x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (LinFun x y) = "(" ++ show x ++ " -o " ++ show y ++ ")"
  show (Pair x y) = "(" ++ show x ++ " , " ++ show y ++ ")" -- Double parens ?
  show (InternalChoice x) = "+{" ++ show (Map.toList x) ++ "}"
  show (ExternalChoice x) = "&{" ++ show (Map.toList x) ++ "}"
  show (Datatype x) =   show (Map.toList x)  -- Datatype ?
  show (Rec s t) = "rec " ++ show s ++ " . " ++ show t
  show (Forall s t) = "forall " ++ show s ++ " . " ++ show t
  show (Var x) = show x


-- read "Skip -o Int -> +{a:Int,b:Bool}" :: Type
-- LinFun Skip (UnFun (Basic Int) (InternalChoice (fromList [("a",Basic Int),("b",Basic Bool)])))
