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
( BasicType(..),
  Type(..),
  TypeMap(..),
  Id,
  Field
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- BASIC TYPES

data BasicType =
  IntType |
  CharType |
  BoolType |
  UnitType
  deriving (Eq, Ord)

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
  deriving Ord
  -- deriving (Eq)
  -- deriving (Show)
  -- deriving (Eq,Show)


type Id = String
type Field = String
type TypeMap = Map.Map Field Type


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
  show (Datatype x) =   show (Map.toList x)
  show (Rec s t) = "rec " ++ show s ++ " . " ++ show t
  show (Forall s t) = "forall " ++ show s ++ " . " ++ show t
  show (Var x) = show x

instance Eq Type where
  (==) = equals Set.empty

equals :: Set.Set (Id, Id) -> Type -> Type -> Bool
equals m Skip Skip = True
equals m (Var x) (Var y)
          | x == y = True
          | otherwise = Set.member (x,y) m
equals m (Forall x t)(Forall y u) = equals (Set.insert (x,y) m) t u
equals m (Rec x t)(Rec y u) = equals (Set.insert (x,y) m) t u
equals m (Semi t1 t2)(Semi v1 v2) = equals m t1 v1 && equals m t2 v2
equals m (Basic x)(Basic y) = x == y
equals m (Out x) (Out y) = x == y
equals m (In x) (In y) = x == y
equals m (LinFun s t)(LinFun u v) = equals m s u && equals m t v
equals m (UnFun s t)(UnFun u v) = equals m s u && equals m t v
equals m (Pair s t)(Pair u v) = equals m s u && equals m t v
equals m (Datatype m1)(Datatype m2) = verifyListEquality m (Map.toList m1) (Map.toList m2)
equals m (InternalChoice m1)(InternalChoice m2) = verifyListEquality m (Map.toList m1) (Map.toList m2)
equals m (ExternalChoice m1)(ExternalChoice m2) = verifyListEquality m (Map.toList m1) (Map.toList m2)

verifyListEquality :: Set.Set (Id, Id) -> [(Id, Type)] -> [(Id, Type)] -> Bool
verifyListEquality _ [] _ = True
verifyListEquality _ _ [] = True
verifyListEquality m (m1:ms1)(m2:ms2) =
  equals m (snd m1) (snd m2) && verifyListEquality m ms1 ms2





-- equals m _ _ = False
