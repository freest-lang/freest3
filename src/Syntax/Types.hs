{- |
Module      :  Syntax.Types
Description :  The language types.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types, duality and equality.
-}

module Syntax.Types
( BasicType(..)
, TypeMap(..)
, Polarity(..)
, Type(..)
, Dual(..)
) where

import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables (ProgVar)
import           Syntax.Base
import qualified Data.Map.Strict as Map


-- The dual function on types, etc
class Dual t where
  dual :: t -> t

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

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
  | Rec Pos TypeVarBind Type 
  -- Functional or session
  | TypeVar Pos TypeVar  -- a recursion variable if bound, polymorphic otherwise
  -- Type operators
  | TypeName Pos TypeVar -- a named type, to be looked upon in a map of type names to types
  | Dualof Pos Type      -- to be expanded into a session type
  deriving (Eq, Ord)

type TypeMap = Map.Map ProgVar Type

instance Position Type where
  -- Functional types
  position (Basic p _)      = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  -- Session types
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  -- Functional or session
  position (Rec p _ _)        = p
  position (TypeVar p _)      = p
  -- Type operators
  position (Dualof p _)       = p
  position (TypeName p _)     = p

instance Dual Type where
  -- Session types
  dual (Semi p t1 t2)   = Semi p (dual t1) (dual t2)
  -- dual (Semi p t1 t2)  = Semi p (Dualof p t1) (Dualof p t2) -- The lazy version loops
  dual (Message p v b)  = Message p (dual v) b
  dual (Choice p v m)   = Choice p (dual v) (Map.map dual m)
  -- dual (Choice p v m)  = Choice p (dual v) (Map.map (Dualof p) m) -- The lazy version loops
  dual (Rec p x t)      = Rec p x (dual t)
  -- dual (Rec p x t)     = Rec p x (Dualof p t) -- The lazy version loops
  -- Type operators
  dual (Dualof _ t)     = t
  dual t@(TypeName _ x) = t -- TODO: This can't be right
  -- Functional types, Skip, TypeVar
  dual t                = t

instance Default Type where
  omission p = Basic p IntType

{- Type equality, up to alpha-conversion

instance Eq Type where 
  t == u = equalTypes Map.empty t u 

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
equalTypes s (Rec _ (TypeVarBind _ x k) t) (Rec _ (TypeVarBind _ y l) u) =
  k ==l && equalTypes (Map.insert x y s) t u
  -- Functional or session
equalTypes s (TypeVar _ x)    (TypeVar _ y)    = equalVars (Map.lookup x s) x y
  -- Type operators
equalTypes s (Dualof _ t)     (Dualof _ u)     = t == u
equalTypes s (TypeName _ x)   (TypeName _ y)   = x == y
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
-}
