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
( Dual(..)
, TypeVar
, KBind(..)
, BasicType(..)
, TypeMap(..)
, Polarity(..)
, Type(..)
, TypeScheme(..)
, free
, subs
, unfold
, isPreSession
, toList -- TODO: not quite sure this belongs here
) where

import           Parse.Lexer (Position, Pos, position)
import           Syntax.Bind
import           Syntax.Kinds
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- DUALITY

class Dual t where
  dual :: t -> t

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

instance Show Polarity where
  show In  = "?"
  show Out = "!"

instance Dual Polarity where
  dual In  = Out
  dual Out = In

-- BASIC TYPES

data BasicType =
    IntType
  | CharType
  | BoolType
  | UnitType
  deriving (Eq, Ord)

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- TYPES

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
  | Choice Pos Polarity TypeMap
  | Rec Pos KBind Type
  -- Functional or session
  | Var Pos TypeVar -- a recursion variable if bound, polymorphic otherwise
  -- Type operators
  | Dualof Pos Type -- to be expanded
  | Name Pos TVar   -- a named type, to be looked upon in a map of Cons to Type
  deriving Ord

type TypeMap = Map.Map PBind Type -- TODO: rename to FieldMap

instance Eq Type where -- Type equality, up to alpha-conversion
  t == u = equalTypes Map.empty (normalise t) (normalise u) 

equalTypes :: Map.Map TypeVar TypeVar -> Type -> Type -> Bool
  -- Functional types
equalTypes s (Basic _ x)      (Basic _ y)      = x == y
equalTypes s (Fun _ m t u)    (Fun _ n v w)    = m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType _ t u) (PairType _ v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype _ m1)  (Datatype _ m2)  = equalMaps s m1 m2
  -- Session types
equalTypes s (Skip _)         (Skip _)         = True
equalTypes s (Semi _ t1 t2)   (Semi _ u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Message _ p x)  (Message _ q y)  = p == q && x == y
equalTypes s (Choice _ v1 m1) (Choice _ v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes s (Rec _ (KBind _ x _) t) (Rec _ (KBind _ y _) u) = equalTypes (Map.insert x y s) t u
  -- Functional or session
equalTypes s (Var _ x)        (Var _ y)        = equalVars (Map.lookup x s) x y
  -- Type operators
equalTypes s (Dualof _ t)     (Dualof _ u)     = t == u
equalTypes s (Name _ x)       (Name _ y)       = x == y
  -- Otherwise
equalTypes _ _              _                  = False

equalVars :: Maybe TypeVar -> TypeVar -> TypeVar -> Bool
equalVars Nothing  y z = y == z
equalVars (Just x) _ z = x == z

equalMaps :: Map.Map TypeVar TypeVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

instance Show Type where
  -- Functional types
  show (Basic _ b)      = show b
  show (Fun _ m t u)    = "(" ++ show t ++ showFunOp m ++ show u ++ ")"
  show (PairType _ t u) = "(" ++ show t ++ ", " ++ show u ++ ")"
  show (Datatype _ m)   = "["++ showMap m ++"]"
  -- Session types
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Choice _ v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Rec _ x t)      = "(rec " ++ show x ++ " . " ++ show t ++ ")"
  -- Functional or session
  show (Var _ x)        = x
  -- Type operators
  show (Dualof _ s)     = "(dualof " ++ show s ++ ")"
  show (Name _ x)       = x
  
showFunOp :: Multiplicity -> String
showFunOp Lin = " -o "
showFunOp Un  = " -> "

showChoice :: Polarity -> String
showChoice In  = "&"
showChoice Out = "+"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (b, v) = show b ++ ": " ++ show v

instance Position Type where
  -- Functional types
  position (Basic p _)     = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  -- Session types
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  position (Rec p _ _)      = p
  -- Functional or session
  position (Var p _)        = p
  -- Type operators
  position (Dualof p _)     = p
  position (Name p _)       = p

instance Dual Type where
  -- Session types
  dual (Semi p t1 t2)  = Semi p (dual t1) (dual t2)
  dual (Message p v b) = Message p (dual v) b
--  dual (Choice p v m)  = Choice p (dual v) (Map.map dual m)
  dual (Choice p v m)  = Choice p (dual v) (Map.map (Dualof p) m) -- The lazy version, hopefully faster
--  dual (Rec p x t)     = Rec p x (dual t)
  dual (Rec p x t)     = Rec p x (Dualof p t) -- The lazy version, hopefully faster
  -- Type operators
  dual (Dualof _ t)    = t
  -- Functional types, Skip, Var, Name
  dual t               = t

-- KINDED BIND

type TypeVar = String

data KBind = KBind Pos TVar Kind

instance Eq KBind where
  (KBind _ x _) == (KBind _ y _) = x == y

instance Ord KBind where
  (KBind _ x _) `compare` (KBind _ y _) = x `compare` y

instance Show KBind where
  show (KBind _ x k) = x ++ " : " ++ show k

instance Position KBind where
  position (KBind p _ _) = p

-- TYPE SCHEMES

data TypeScheme = TypeScheme Pos [KBind] Type

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

toList :: TypeScheme -> [TypeScheme] -- TODO: return [Type]
toList (TypeScheme p b (Fun _ _ t1 t2)) = (TypeScheme p b t1) : toList (TypeScheme p b t2)
toList t = [t]

-- UNFOLDING, SUBSTITUTING

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec p x t) = subs (Rec p x t) x t

-- [u/x]t, substitute u for x on t
subs :: Type -> KBind -> Type -> Type 
  -- Functional types
subs t y (Fun p m t1 t2)    = Fun p m (subs t y t1) (subs t y t2)
subs t y (PairType p t1 t2) = PairType p (subs t y t1) (subs t y t2)
subs t y (Datatype p m)     = Datatype p (Map.map(subs t y) m)
  -- Session types
subs t y (Semi p t1 t2)     = Semi p (subs t y t1) (subs t y t2)
subs t y (Choice p v m)     = Choice p v (Map.map(subs t y) m)
subs t2 y (Rec p x t1)      -- Assume y /= x
  | x == y                  = Rec p x t1
  | otherwise               = Rec p x (subs t2 y t1)
  -- Functional or session
subs t (KBind _ y _) (Var p x)
  | x == y                  = t
  | otherwise               = Var p x
  -- Type operators  
subs t y (Dualof p t1)      = Dualof p (subs t y t1)
  -- Otherwise: Basic, Skip, Message, Name
subs _ _ t                  = t

-- The set of free type variables in a type
free :: Type -> Set.Set TVar
  -- Functional types
free (Basic _ _)      = Set.empty
free (Fun _ _ t u)    = Set.union (free t) (free u)
free (PairType _ t u) = Set.union (free t) (free u)
free (Datatype _ m)   = Map.foldr (Set.union . free) Set.empty m
  -- Session types
free (Skip _)         = Set.empty
free (Semi _ t u)     = Set.union (free t) (free u)
free (Message _ _ _)  = Set.empty
free (Choice _ _ m)   = Map.foldr (Set.union . free) Set.empty m
free (Rec _ (KBind _ x _) t) = Set.delete x (free t)
  -- Functional or session
free (Var _ x)        = Set.singleton x
  -- Type operators
free (Dualof _ t)     = free t
free (Name _ _)       = Set.empty

normalise :: Type -> Type
  -- Functional types
normalise (Fun p q t u)    = Fun p q (normalise t) (normalise u)
normalise (PairType p t u) = PairType p (normalise t) (normalise u)
normalise (Datatype p m)   = Datatype p (Map.map normalise m)
  -- Session types
normalise (Semi p (Choice q v m) t) =
  Choice q v (Map.map (\v -> append (normalise v) u) m)
  where u = normalise t
normalise (Semi p t u)     = append (normalise t) (normalise u)
normalise (Choice p q m)   = Choice p q (Map.map normalise m)
normalise (Rec p (KBind q x k) t)
  | x `Set.member` (free t) = Rec p (KBind q x k) u
  | otherwise               = u
  where u = normalise t
  -- Functional or session
  -- Type operators
normalise (Dualof _ t)     = normalise (dual t)
  -- Otherwise: Basic, Skip, Message, Var, Name
normalise t                = t
-- Note: we could be more ambitious and go after the Name types, but we'd need a TypeEnv

append :: Type -> Type -> Type
append (Skip _)       t = t
append t       (Skip _) = t
append (Semi p t u)   v = Semi p t (append u v)
append (Choice q v m) t = Choice q v (Map.map (`append` t) m)
append t              u = Semi (position t) t u

-- SESSION TYPES

-- Is this type a pre session type? (a session type that is
-- syntactically correct, but not necessarilty well-kinded)
isPreSession :: Type -> KindEnv -> Bool
  -- Session types
isPreSession (Skip _) _        = True
isPreSession (Semi _ _ _) _    = True
isPreSession (Message _ _ _) _ = True
isPreSession (Choice _ _ _) _  = True
isPreSession (Rec _ _ _) _     = True
  -- Functional or session
isPreSession (Var p x) kenv    = Map.member (TBind p x) kenv
  -- Type operators
isPreSession (Dualof _ _) _    = True
-- isPreSession (Name _ c) = ... TODO: requires a TypeEnv
  -- Otherwise: Functional types
isPreSession _ _               = False
