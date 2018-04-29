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
import Data.List -- intersperse

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

type TypeMap = Map.Map TypeVar Type

data ChoiceView =
    External
  | Internal
  deriving (Eq, Ord)

instance Show ChoiceView where
  show External = "&"
  show Internal = "+"

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
  | Rec TypeVar Kind Type    -- TODO: Use 'Rec Bind Type'
  | Forall TypeVar Kind Type -- TODO: remove, use TypeScheme instead
  | Var TypeVar
  deriving Ord

instance Eq Type where
  (==) = equalTypes Set.empty

equalTypes :: Set.Set (TypeVar, TypeVar) -> Type -> Type -> Bool
equalTypes s Skip           Skip           = True
equalTypes s (Var x)        (Var y)        = x == y || Set.member (x, y) s
equalTypes s (Forall x k t) (Forall y w u) =
  k == w && equalTypes (Set.insert (x, y) s) t u
equalTypes s (Rec x k t)    (Rec y w u)    =
  k == w && equalTypes (Set.insert (x, y) s) t u
equalTypes s (Semi t1 t2)   (Semi u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Basic x)      (Basic y)      = x == y
equalTypes s (Out x)        (Out y)        = x == y
equalTypes s (In x)         (In y)         = x == y
equalTypes s (Fun m t u)    (Fun n v w)    =
  m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType t u) (PairType v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype m1)  (Datatype m2)  = equalMaps s m1 m2
equalTypes s (Choice v1 m1) (Choice v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes _ _              _              = False

equalMaps :: Set.Set (TypeVar, TypeVar) -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

instance Show Type where
  show (Basic b)      = show b
  show Skip           = "Skip"
  show (Semi t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Out b)        = "!" ++ show b
  show (In b)         = "?" ++ show b
  show (Fun Lin t u)  = showFun t "-o" u
  show (Fun Un t u)   = showFun t "->" u
  show (PairType t u) = "(" ++  show t ++ ", " ++ show u ++ ")"
  show (Choice v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Datatype m)   = "["++ showMap m ++"]"
  show (Rec x k t)    = "(rec " ++ x ++ " :: " ++ show k ++" . " ++ show t ++ ")"
  show (Forall x k t) = "(forall " ++ x ++ " :: " ++ show k ++ " => " ++ show t ++ ")"
  show (Var s)        = s

showFun :: Type -> String -> Type -> String
showFun t op u = "(" ++ show t ++ " " ++ op ++ " " ++ show u ++ ")"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (k, v) = k ++ ": " ++ show v

-- TYPE VARIABLE BINDING

data Bind = Bind {var :: TypeVar, kind :: Kind}
  deriving (Eq, Ord)

instance Show Bind where
  show b = var b ++ " :: " ++ show (kind b)

-- TYPE SCHEMES

data TypeScheme =
    Polymorphic Bind TypeScheme
  | Monomorphic Type
  deriving Ord

instance Eq TypeScheme where
  (==) = equalSchemes Set.empty

equalSchemes :: Set.Set (TypeVar, TypeVar) -> TypeScheme -> TypeScheme -> Bool
equalSchemes s (Monomorphic t)   (Monomorphic u)   = equalTypes s t u
equalSchemes s (Polymorphic b t) (Polymorphic c u) =
  kind b == kind c && equalSchemes (Set.insert (var b, var c) s) t u

instance Show TypeScheme where
  show (Monomorphic t)   = show t
  show (Polymorphic b s) = "forall " ++ show b ++ " => " ++ show s

-- DUALITY

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var v)      = Var v
dual Skip         = Skip
dual (Out b)      = In b
dual (In b)       = Out b
dual (Choice v m) = Choice (dualChoice v) (Map.map dual m)
dual (Semi t1 t2) = Semi (dual t1) (dual t2)
dual (Rec x k t)  = Rec x k (dual t)

dualChoice :: ChoiceView -> ChoiceView
dualChoice External = Internal
dualChoice Internal = External

toList :: Type -> [Type]
toList (Fun _ t1 t2) = t1 : toList t2
toList (Forall _ _ t) = toList t
-- toList (Semi t1 t2) = toList t1 ++ toList t2
toList t = [t]
