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
, KBind(..)
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
, isPreSession
) where

import           Syntax.Position
import           Syntax.Kinds
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

instance Show Polarity where
  show In = "?"
  show Out = "!"

-- CHOICE

data ChoiceView =
    External
  | Internal
  deriving (Eq, Ord)

instance Show ChoiceView where
  show External = "&"
  show Internal = "+"

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

-- TYPES

type Constructor = Var -- = String

type TypeMap = Map.Map Bind Type

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
  | Rec Pos TypeVar Type
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
equalTypes s (Rec _ x t)      (Rec _ y u)      = equalTypes (insertTypeVar (x, y) s) t u
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
equalVars Nothing  y z = y == z
equalVars (Just x) _ z = x == z

equalMaps :: Map.Map TypeVar TypeVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

insertTypeVar :: (TypeVar, TypeVar) -> Map.Map TypeVar TypeVar -> Map.Map TypeVar TypeVar
insertTypeVar (b, c) = Map.insert b c

-- Showing a type
instance Show Type where
  show (Basic _ b)      = show b
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Fun _ m t u)    = "(" ++ show t ++ showFunOp m ++ show u ++ ")"
  show (PairType _ t u) = "(" ++ show t ++ ", " ++ show u ++ ")"
  show (Choice _ v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Datatype _ m)   = "["++ showMap m ++"]"
  show (Rec _ x t)      = "(rec " ++ x ++ " . " ++ show t ++ ")"
  show (Var _ s)        = s
  show (Dualof _ s)     = "dualof " ++ show s

showFunOp :: Multiplicity -> String
showFunOp Lin = " -o "
showFunOp Un  = " -> "

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (Bind _ k, v) = k ++ ": " ++ show v

instance Position Type where
  position (Basic p _)     = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  position (Rec p _ _)      = p
  position (Var p _)        = p
  position (Dualof p _)     = p

-- KINDED BIND

type TypeVar = String

data KBind = KBind Pos TypeVar Kind

instance Show KBind where
  show (KBind _ x k) = x ++ " : " ++ show k

-- TYPE SCHEMES

data TypeScheme = TypeScheme Pos [KBind] Type
{-
instance Eq TypeScheme where -- TODO: Remove
  (==) = equalSchemes Map.empty

equalSchemes :: Map.Map TypeVar TypeVar -> TypeScheme -> TypeScheme -> Bool
equalSchemes s (TypeScheme _ bs t) (TypeScheme _ cs u) =
{-  bs == cs && -} equalTypes (insertBinds bs cs s) t u

insertBinds :: [Bind] -> [Bind] -> Map.Map TypeVar TypeVar -> Map.Map TypeVar TypeVar
insertBinds bs cs s = foldr insertBind s (zip bs cs)

insertBind :: (Bind, Bind) -> Map.Map TypeVar TypeVar -> Map.Map TypeVar TypeVar
insertBind (Bind _ x _, Bind _ y _) = Map.insert x y
-}
instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ showBindings bs ++ " => " ++ show t

showBindings :: [KBind] -> String
showBindings bs = concat $ intersperse ", " (map show bs)

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

-- DUALITY

-- The dual of a session type
-- Assume that the type is a Session Type
dual :: Type -> Type
dual (Var p v)         = Var p v
dual (Skip p)          = Skip p
dual (Message pos p b) = Message pos (dualPolarity p) b
dual (Choice pos p m)  = Choice pos (dualView p) (Map.map dual m)
dual (Semi p t1 t2)    = Semi p (dual t1) (dual t2)
dual (Rec p x t)       = Rec p x (dual t)
dual (Dualof _ t)      = t

dualPolarity :: Polarity -> Polarity
dualPolarity In  = Out
dualPolarity Out = In

dualView :: ChoiceView -> ChoiceView
dualView External = Internal
dualView Internal = External

toList :: TypeScheme -> [TypeScheme] -- TODO: what for?
toList (TypeScheme p b (Fun _ _ t1 t2)) = (TypeScheme p b t1) : toList (TypeScheme p b t2)
toList t = [t]

-- UNFOLDING, RENAMING, SUBSTITUTING

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec p x t) = subs (Rec p x t) x t

rename :: Type -> TypeVar -> Type
-- Assumes parameter is a Rec type
-- rename (Rec p (Bind x pb k) t) y = Rec p (Bind y pb k) (subs (Var p y) x t)
rename (Rec p x t) y = Rec p y (subs (Var p y) x t)

-- [u/x]t, substitute u for x on t
subs :: Type -> TypeVar -> Type -> Type 
subs t y (Var p x)
    | x == y                = t
    | otherwise             = Var p x
subs t y (Semi p t1 t2)     = Semi p (subs t y t1) (subs t y t2)
subs t y (PairType p t1 t2) = PairType p (subs t y t1) (subs t y t2)
-- Assume y /= x
subs t2 y (Rec p x t1)
  | x == y                  = Rec p x t1
  | otherwise               = Rec p x (subs t2 y t1)
subs t y (Choice p v m)     = Choice p v (Map.map(subs t y) m)
subs t y (Fun p m t1 t2)    = Fun p m (subs t y t1) (subs t y t2)
subs _ _ t                  = t

subL :: Type -> [(Type,KBind)] -> Type
subL t bs =
  foldr (\(u, (KBind _ x _)) acc -> subs u x acc) t bs

-- SESSION TYPES

-- Is this type a pre session type? (a session type that is
-- syntactically correct, but not necessarilty well-kinded)
-- isPreSession :: Type -> KindEnv -> Bool -- TODO: import loop
isPreSession :: Type -> Map.Map Bind (Pos,Kind) -> Bool
isPreSession (Skip _) _        = True
isPreSession (Semi _ _ _) _    = True
isPreSession (Message _ _ _) _ = True
isPreSession (Choice _ _ _) _ = True
isPreSession (Rec _ _ _) _ = True
isPreSession (Var p x) kenv    = Map.member (Bind p x) kenv
isPreSession _ _ = False
