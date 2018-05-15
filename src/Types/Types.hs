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
( TypeVar
, Bind(..)
, BasicType(..)
, Constructor
, TypeMap(..)
, ChoiceView(..)
, Polarity(..)
, Type(..)
, TypeScheme(..)
, dual
, toList
) where

import Types.Kinds
import qualified Data.Map.Strict as Map
import Data.List (intersperse)

-- TYPE VARIABLE BINDINGS

type TypeVar = String

data Bind = Bind {var :: TypeVar, kind :: Kind}
  deriving (Ord)

instance Eq Bind where
  b == c = kind b == kind c

instance Show Bind where
  show b = var b ++ " :: " ++ show (kind b)

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

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

instance Show Polarity where
  show In = "?"
  show Out = "!"
 
-- Choice View

data ChoiceView =
    External
  | Internal
  deriving (Eq, Ord)

instance Show ChoiceView where
  show External = "&"
  show Internal = "+"
 
-- TYPES

type Constructor = String

type TypeMap = Map.Map Constructor Type

data Type =
  -- Functional types
    Basic BasicType
  | Fun Multiplicity Type Type
  | PairType Type Type
  | Datatype TypeMap
  -- Session types
  | Skip
  | Semi Type Type
  | Message Polarity BasicType
  | Choice ChoiceView TypeMap
  | Rec Bind Type
  -- | Forall TypeVar Kind Type -- TODO: remove, use TypeScheme instead
  | Var TypeVar
  deriving (Ord)

-- Type equality

instance Eq Type where
  (==) = equalTypes Map.empty

equalTypes :: Map.Map TypeVar TypeVar -> Type -> Type -> Bool
equalTypes s Skip           Skip           = True
equalTypes s (Var x)        (Var y)        = equalVars (Map.lookup x s) x y
--equalTypes s (Forall x k t) (Forall y w u) = k == w && equalTypes (Map.insert x y s) t u
equalTypes s (Rec b t)      (Rec c u)      = b == c && equalTypes (insertBind (b, c) s) t u
equalTypes s (Semi t1 t2)   (Semi u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Basic x)      (Basic y)      = x == y
equalTypes s (Message p x)  (Message q y)  = p == q && x == y
equalTypes s (Fun m t u)    (Fun n v w)    = m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType t u) (PairType v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype m1)  (Datatype m2)  = equalMaps s m1 m2
equalTypes s (Choice v1 m1) (Choice v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes _ _              _              = False

equalVars :: Maybe TypeVar -> TypeVar -> TypeVar -> Bool
equalVars Nothing  x y = x == y
equalVars (Just z) _ y = z == y

equalMaps :: Map.Map TypeVar TypeVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

-- Type show

instance Show Type where
  show (Basic b)      = show b
  show Skip           = "Skip"
  show (Semi t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message Out b)= "!" ++ show b
  show (Message In b) = "?" ++ show b
  show (Fun Lin t u)  = showFun t "-o" u
  show (Fun Un t u)   = showFun t "->" u
  show (PairType t u) = "(" ++  show t ++ ", " ++ show u ++ ")"
  show (Choice v m) = show v ++ "{" ++ showMap m ++ "}"
  show (Datatype m)   = "["++ showMap m ++"]"
  show (Rec b t)      = "(rec " ++ show b ++ " . " ++ show t ++ ")"
--  show (Forall x k t) = "(forall " ++ x ++ " :: " ++ show k ++ " => " ++ show t ++ ")"
  show (Var s)        = s

showFun :: Type -> String -> Type -> String
showFun t op u = "(" ++ show t ++ " " ++ op ++ " " ++ show u ++ ")"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (k, v) = k ++ ": " ++ show v

-- TYPE SCHEMES

data TypeScheme = TypeScheme [Bind] Type deriving Ord

instance Eq TypeScheme where
  (==) = equalSchemes Map.empty

equalSchemes :: Map.Map TypeVar TypeVar -> TypeScheme -> TypeScheme -> Bool
equalSchemes s (TypeScheme bs t) (TypeScheme cs u) =
  bs == cs && equalTypes (insertBinds bs cs s) t u

insertBinds :: [Bind] -> [Bind] -> Map.Map TypeVar TypeVar -> Map.Map TypeVar TypeVar
insertBinds bs cs s = foldr insertBind s (zip bs cs)

insertBind :: (Bind, Bind) -> Map.Map TypeVar TypeVar -> Map.Map TypeVar TypeVar
insertBind (b, c) = Map.insert (var b) (var c)

instance Show TypeScheme where
  show (TypeScheme bs t) = showTypeScheme bs t

showTypeScheme :: [Bind] -> Type -> String
showTypeScheme [] t = show t
showTypeScheme bs t = "forall " ++ showBindings bs ++ " => " ++ show t

showBindings :: [Bind] -> String
showBindings bs = concat $ intersperse ", " (map show bs)

{- Alternative:
data TypeScheme =
    Polymorphic Bind TypeScheme
  | Monomorphic Type
  deriving Ord

instance Eq TypeScheme where
  (==) = equalSchemes Map.empty

equalSchemes :: Map.Set (TypeVar, TypeVar) -> TypeScheme -> TypeScheme -> Bool
equalSchemes s (Monomorphic t)   (Monomorphic u)   = equalTypes s t u
equalSchemes s (Polymorphic b t) (Polymorphic c u) =
  kind b == kind c && equalSchemes (Map.insert (var b, var c) s) t u

instance Show TypeScheme where
  show (Monomorphic t)   = show t
  show (Polymorphic b s) = "forall " ++ show b ++ " => " ++ show s
-}

-- DUALITY

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var v)      = Var v
dual Skip         = Skip
dual (Message p b)= Message (dualPolarity p) b
dual (Choice p m) = Choice (dualView p) (Map.map dual m)
dual (Semi t1 t2) = Semi (dual t1) (dual t2)
dual (Rec b t)    = Rec b (dual t)

dualPolarity :: Polarity -> Polarity
dualPolarity In = Out
dualPolarity Out = In

dualView :: ChoiceView -> ChoiceView
dualView External = Internal
dualView Internal = External

toList :: TypeScheme -> [TypeScheme]
toList (TypeScheme b (Fun _ t1 t2)) = (TypeScheme b t1) : toList (TypeScheme b t2)
-- toList (Forall _ _ t) = toList t
toList t = [t]

{- WAS:
toList :: Type -> [Type]
toList (Fun _ t1 t2) = t1 : toList t2
toList (Forall _ _ t) = toList t
toList t = [t]
-}
