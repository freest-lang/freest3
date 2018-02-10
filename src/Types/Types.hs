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
  Field,
  dual
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- BASIC TYPES

data BasicType =
          IntType  |
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
  Rec Id Type |
  Forall Id {- Kind TODO-} Type |
  Var Id
  deriving Ord
  -- deriving (Eq)
  -- deriving (Ord,Show)
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
  show Skip = id "Skip"
  show (Semi x y) = "(" ++ show x ++ ";" ++ show y ++ ")"
  show (Out x) = "!" ++ show x
  show (In x) = "?" ++ show x
  show (UnFun x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (LinFun x y) = "(" ++ show x ++ " -o " ++ show y ++ ")"
  show (Pair x y) =  "(" ++  show x ++ " , " ++ show y ++ ")"
  show (InternalChoice x) = "+{" ++ printMap x ++ "}"
  show (ExternalChoice x) = "&{" ++ printMap x ++ "}"
  show (Datatype x) =   "["++ printMap x ++"]"
  show (Rec s t) = "(rec " ++ id s ++ " . " ++ show t ++ ")"
  show (Forall s t) = "(forall " ++ id s ++ " . " ++ show t ++ ")"
  show (Var x) = id x

printMap :: TypeMap -> String
printMap m =
  reverse $ drop 2 $ reverse (Map.foldlWithKey(\acc k v -> acc ++ id k ++ ":" ++ show v ++ ", ") "" m)


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
equals m (Datatype m1)(Datatype m2) = verifyMapEquality m m1 m2 -- verifyListEquality m (Map.toList m1) (Map.toList m2)
equals m (InternalChoice m1)(InternalChoice m2) = verifyMapEquality m m1 m2
equals m (ExternalChoice m1)(ExternalChoice m2) = verifyMapEquality m m1 m2
equals _ _ _ = False

verifyMapEquality :: Set.Set (Id, Id) -> TypeMap -> TypeMap -> Bool
verifyMapEquality m tm1 tm2 =
   Map.size tm1 == Map.size tm2 &&
      Map.foldlWithKey(\b l t -> b && l `Map.member` tm2 && equals m (tm2 Map.! l) t) True tm1

-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var v)            = Var v
dual Skip               = Skip
dual (Out b)            = In b
dual (In b)             = Out b
dual (InternalChoice m) = ExternalChoice $ Map.map (\x -> dual x) m
dual (ExternalChoice m) = InternalChoice $ Map.map (\x -> dual x) m
dual (Semi t1 t2)       = Semi (dual t1) (dual t2)
dual (Rec x t)          = Rec x (dual t)
