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

module Syntax.Types
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
, toList -- TODO: not quite sure this belongs here
, unfold
, rename
, subs
, subL -- TODO: not quite sure this belongs here
, Pos
, typePos
, isPreSession
) where

import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import           Syntax.Kinds

-- POSITION -- TODO: This should be common to all syntax
type Pos = (Int, Int)

-- TYPE VARIABLE BINDINGS

type TypeVar = String

data Bind = Bind {var :: TypeVar, kind :: Kind}
  deriving (Ord)

instance Eq Bind where
  b == c = kind b == kind c

instance Show Bind where
  show b = var b ++ " :: " ++ show (kind b)

-- BASIC TYPES

data BasicType = -- TODO: Add Pos
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
    Basic Pos BasicType
  | Fun Pos Multiplicity Type Type
  | PairType Pos Type Type
  | Datatype Pos TypeMap
  -- Session types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity BasicType
  | Choice Pos ChoiceView TypeMap
  | Rec Pos Bind Type
  -- Functional or Session
  | Var Pos TypeVar
  -- Type operators
  | Dualof Pos Type
  deriving (Ord)

-- Type equality, up to alpha-conversion.
instance Eq Type where
  (==) = equalTypes Map.empty

equalTypes :: Map.Map TypeVar TypeVar -> Type -> Type -> Bool
equalTypes s (Skip _)         (Skip _)         = True
equalTypes s (Var _ x)        (Var _ y)        = equalVars (Map.lookup x s) x y
equalTypes s (Rec _ b t)      (Rec _ c u)      = b == c && equalTypes (insertBind (b, c) s) t u
equalTypes s (Semi _ t1 t2)   (Semi _ u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Basic _ x)      (Basic _ y)      = x == y
equalTypes s (Message _ p x)  (Message _ q y)  = p == q && x == y
equalTypes s (Fun _ m t u)    (Fun _ n v w)    = m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType _ t u) (PairType _ v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype _ m1)  (Datatype _ m2)  = equalMaps s m1 m2
equalTypes s (Choice _ v1 m1) (Choice _ v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes s (Dualof _ t)     (Dualof _ u)     = t == u
equalTypes _ _              _                  = False

equalVars :: Maybe TypeVar -> TypeVar -> TypeVar -> Bool
equalVars Nothing  x y = x == y
equalVars (Just z) _ y = z == y

equalMaps :: Map.Map TypeVar TypeVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

-- Showing a type -- TODO: parenthesis needed
instance Show Type where
  show (Basic _ b)      = show b
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Fun _ Lin t u)  = showFun t "-o" u
  show (Fun _ Un t u)   = showFun t "->" u
  show (PairType _ t u) = "(" ++  show t ++ ", " ++ show u ++ ")"
  show (Choice _ v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Datatype _ m)   = "["++ showMap m ++"]"
  show (Rec _ b t)      = "(rec " ++ show b ++ " . " ++ show t ++ ")"
  show (Var _ s)        = s
  show (Dualof _ s)     = "dualof " ++ show s

showFun :: Type -> String -> Type -> String
showFun t op u = "(" ++ show t ++ " " ++ op ++ " " ++ show u ++ ")"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (k, v) = k ++ ": " ++ show v

-- The position of a type
typePos :: Type -> Pos
typePos (Basic p _) = p
typePos (Fun p _ _ _) = p
typePos (PairType p _ _) = p
typePos (Datatype p _) = p
typePos (Skip p) = p
typePos (Semi p _ _) = p
typePos (Message p _ _) = p
typePos (Choice p _ _) = p
typePos (Rec p _ _) = p
typePos (Var p _) = p
typePos (Dualof p _) = p

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

-- DUALITY

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var p v)         = Var p v
dual (Skip p)          = Skip p
dual (Message pos p b) = Message pos (dualPolarity p) b
dual (Choice pos p m)  = Choice pos (dualView p) (Map.map dual m)
dual (Semi p t1 t2)    = Semi p (dual t1) (dual t2)
dual (Rec p b t)       = Rec p b (dual t)
dual (Dualof _ t)      = t

dualPolarity :: Polarity -> Polarity
dualPolarity In  = Out
dualPolarity Out = In

dualView :: ChoiceView -> ChoiceView
dualView External = Internal
dualView Internal = External

toList :: TypeScheme -> [TypeScheme]
toList (TypeScheme b (Fun _ _ t1 t2)) = (TypeScheme b t1) : toList (TypeScheme b t2)
toList t = [t]

-- UNFOLDING, RENAMING, SUBSTITUTING

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec p b t) = subs (Rec p b t) (var b) t

rename :: Type -> TypeVar -> Type
-- Assumes parameter is a Rec type
rename (Rec p (Bind x k) t) y = Rec p (Bind y k) (subs (Var p y) x t)

-- [u/x]t, substitute u for x on t
subs :: Type -> TypeVar -> Type -> Type 
subs t y (Var p x)
    | x == y                = t
    | otherwise             = Var p x
subs t y (Semi p t1 t2)     = Semi p (subs t y t1) (subs t y t2)
subs t y (PairType p t1 t2) = PairType p (subs t y t1) (subs t y t2)
-- Assume y /= x
subs t2 y (Rec p b t1)
  | var b == y              = Rec p b t1
  | otherwise               = Rec p b (subs t2 y t1)
subs t y (Choice p v m)     = Choice p v (Map.map(subs t y) m)
subs t y (Fun p m t1 t2)    = Fun p m (subs t y t1) (subs t y t2)
subs _ _ t                  = t

subL :: Type -> [(Type,Bind)] -> Type
subL t bs =
  foldr (\(t', b) acc -> subs t' (var b) acc) t bs

-- SESSION TYPES

-- Is this type a pre session type? (a session type that is
-- syntactically correct, but not necessarilty well-kinded)
-- TODO:
-- Map.Map TypeVar (Pos,Kind) stands for KindEnv, can't import it
-- due to a cycle of imports error, some refactor needed.
isPreSession :: Type -> Map.Map TypeVar (Pos,Kind) -> Bool
isPreSession (Skip _) _        = True
isPreSession (Semi _ _ _) _    = True
isPreSession (Message _ _ _) _ = True
isPreSession (Choice _ _ _) _ = True
isPreSession (Rec _ Bind{kind=Kind Session _} _) _ = True
isPreSession (Var _ x) kenv    = Map.member x kenv
isPreSession _ _ = False
