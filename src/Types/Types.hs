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
, Field
, dual
) where

import Types.Kinds
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- BASIC TYPES

data BasicType =
          IntType  |
          CharType |
          BoolType |
          UnitType
          deriving (Eq, Ord)

instance Show BasicType where
  show IntType = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- TYPES

type Id = String
-- The name of a field in a choice or in a datatype
type Field = String -- TODO: rename to Constructor
type TypeMap = Map.Map Field Type

data Type =
  Basic BasicType |
  Skip |
  Semi Type Type |
  Out BasicType |
  In BasicType |
  Fun Multiplicity Type Type |
  Pair Type Type |
  ExternalChoice TypeMap |      -- TODO: merge ExternalChoice and InternalChoice
  InternalChoice TypeMap |
  Datatype TypeMap |
  Rec Id Type |
  Forall Id {- Kind TODO-} Type |
  Var Id
  deriving Ord

instance Show Type where
  show (Basic x) = show x
  show Skip = id "Skip"
  show (Semi x y) = "(" ++ show x ++ ";" ++ show y ++ ")"
  show (Out x) = "!" ++ show x
  show (In x) = "?" ++ show x
  show (Fun Lin x y) = showFun "-o" x y
  show (Fun  Un x y) = showFun "->" x y
  show (Pair x y) =  "(" ++  show x ++ " , " ++ show y ++ ")"
  show (InternalChoice x) = "+{" ++ showMap x ++ "}"
  show (ExternalChoice x) = "&{" ++ showMap x ++ "}"
  show (Datatype x) =   "["++ showMap x ++"]"
  show (Rec s t) = "(rec " ++ id s ++ " . " ++ show t ++ ")"
  show (Forall s t) = "(forall " ++ id s ++ " . " ++ show t ++ ")"
  show (Var x) = id x

showFun :: String -> Type -> Type -> String
showFun op left right = "(" ++ show left ++ " " ++ op ++ " " ++ show right ++ ")"

showMap :: TypeMap -> String
showMap m =
  reverse $ drop 2 $ reverse (Map.foldlWithKey(\acc k v -> acc ++ id k ++ ":" ++ show v ++ ", ") "" m)

instance Eq Type where
  (==) = equals Set.empty

equals :: Set.Set (Id, Id) -> Type -> Type -> Bool
equals s Skip Skip = True
equals s (Var x) (Var y)
    | x == y = True
    | otherwise = Set.member (x,y) s
equals s (Forall x t)(Forall y u) = equals (Set.insert (x,y) s) t u
equals s (Rec x t)(Rec y u) = equals (Set.insert (x,y) s) t u
equals s (Semi t1 t2)(Semi v1 v2) = equals s t1 v1 && equals s t2 v2
equals s (Basic x)(Basic y) = x == y
equals s (Out x) (Out y) = x == y
equals s (In x) (In y) = x == y
equals s (Fun m t u) (Fun n v w) = m == n && equals s t v && equals s u w
equals s (Pair t u)(Pair v w) = equals s t v && equals s u w
equals s (Datatype m1)(Datatype m2) = equalMaps s m1 m2 -- verifyListEquality m (Map.toList m1) (Map.toList m2)
equals s (InternalChoice m1)(InternalChoice m2) = equalMaps s m1 m2
equals s (ExternalChoice m1)(ExternalChoice m2) = equalMaps s m1 m2
equals _ _ _ = False

equalMaps :: Set.Set (Id, Id) -> TypeMap -> TypeMap -> Bool
equalMaps m tm1 tm2 =
   Map.size tm1 == Map.size tm2 &&
      Map.foldlWithKey(\b l t -> b && l `Map.member` tm2 && equals m (tm2 Map.! l) t) True tm1

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var v)            = Var v
dual Skip               = Skip
dual (Out b)            = In b
dual (In b)             = Out b
dual (InternalChoice m) = ExternalChoice (Map.map dual m)
dual (ExternalChoice m) = InternalChoice (Map.map dual m)
dual (Semi t1 t2)       = Semi (dual t1) (dual t2)
dual (Rec x t)          = Rec x (dual t)
