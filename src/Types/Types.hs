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
, TypeVar
, ChoiceView(..)
, dual
, toList
) where

import Types.Kinds
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- BASIC TYPES

data BasicType =
    IntType
  | CharType
  | BoolType
  | UnitType
  deriving (Eq, Ord)

instance Show BasicType where
  show IntType = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- TYPES

type TypeVar = String


-- Const name
-- value -> Constructor

type TypeMap = Map.Map TypeVar Type

data ChoiceView = External | Internal deriving (Eq, Ord, Show)

data Type =
    Basic BasicType
  | Skip
  | Semi Type Type
  | Out BasicType
  | In BasicType
  | Fun Multiplicity Type Type
  | PairType Type Type
  | Choice ChoiceView TypeMap
  | Datatype TypeMap
  | Rec TypeVar Kind Type
  | Forall TypeVar Kind Type
  | Var TypeVar
  deriving Ord

instance Show Type where
  show (Basic x) = show x
  show Skip = "Skip"
  show (Semi t u) = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Out b) = "!" ++ show b
  show (In b) = "?" ++ show b
  show (Fun Lin t u) = showFun "-o" t u
  show (Fun Un t u) = showFun "->" t u
  show (PairType t u) =  "(" ++  show t ++ ", " ++ show u ++ ")"
  show (Choice Internal t) = showChoice "+" t
  show (Choice External t) = showChoice "&" t
  show (Datatype t) =   "["++ showMap t ++"]"
  show (Rec s k t) = "(rec " ++ id s ++ " :: " ++ show k ++" . " ++ show t ++ ")"
  show (Forall s k t) = "(forall " ++ id s ++ " :: " ++ show k ++ " => " ++ show t ++ ")"
  show (Var s) = id s

showChoice :: String -> TypeMap -> String
showChoice op map = op ++ "{" ++ showMap map ++ "}"

showFun :: String -> Type -> Type -> String
showFun op left right = "(" ++ show left ++ " " ++ op ++ " " ++ show right ++ ")"

showMap :: TypeMap -> String
showMap m =
  reverse $ drop 2 $ reverse (Map.foldlWithKey(\acc k v -> acc ++ id k ++ ":" ++ show v ++ ", ") "" m)

instance Eq Type where
  (==) = equals Set.empty

equals :: Set.Set (TypeVar, TypeVar) -> Type -> Type -> Bool
equals s Skip           Skip           = True
equals s (Var x)        (Var y)        = x == y || Set.member (x, y) s
equals s (Forall x k t) (Forall y w u) =
  k == w && equals (Set.insert (x, y) s) t u
equals s (Rec x k t)    (Rec y w u)    =
  k == w && equals (Set.insert (x, y) s) t u
equals s (Semi t1 t2)   (Semi u1 u2)   = equals s t1 u1 && equals s t2 u2
equals s (Basic x)      (Basic y)      = x == y
equals s (Out x)        (Out y)        = x == y
equals s (In x)         (In y)         = x == y
equals s (Fun m t u)    (Fun n v w)    =
  m == n && equals s t v && equals s u w
equals s (PairType t u) (PairType v w) = equals s t v && equals s u w
equals s (Datatype m1)  (Datatype m2)  = equalMaps s m1 m2
equals s (Choice v1 m1) (Choice v2 m2) = v1 == v2 && equalMaps s m1 m2
equals _ _              _              = False

equalMaps :: Set.Set (TypeVar, TypeVar) -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equals s t (m2 Map.! l)) True m1

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var v)      = Var v
dual Skip         = Skip
dual (Out b)      = In b
dual (In b)       = Out b
dual (Choice v m) = Choice (dualChoice v) (Map.map dual m)
dual (Semi t1 t2) = Semi (dual t1) (dual t2)
dual (Rec x k t)    = Rec x k (dual t)

dualChoice :: ChoiceView -> ChoiceView
dualChoice External = Internal
dualChoice Internal = External

toList :: Type -> [Type]
toList (Fun _ t1 t2) = t1 : toList t2
toList (Forall _ _ t) = toList t
-- toList (Semi t1 t2) = toList t1 ++ toList t2
toList t = [t]
