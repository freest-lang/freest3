{- |
Module      :  Syntax.Types
Description :  The language types.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types and equality.
-}

module Syntax.Types
( -- BasicType(..),
  Type(..)
, TypeBind(..)
, TypeMap
, Polarity(..)
, TypeOpsEnv
, TypeEnv
, VarEnv
) where

import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables (ProgVar)
import           Syntax.Base
import qualified Data.Map.Strict as Map

-- POSITIONS & TYPE OPERATORS (TYPENAME & DUALOF)

type TypeOpsEnv = Map.Map Pos Type

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

-- BASIC TYPES

-- data BasicType =
--     IntType
--   | CharType
--   | BoolType
--   | UnitType
--   deriving (Eq, Ord)

-- TYPES

data Type =
  -- Functional Types
    IntType Pos
  | CharType Pos
  | BoolType Pos
  | UnitType Pos
  | Fun Pos Multiplicity Type Type
  | PairType Pos Type Type      -- TODO: Rename to Pair, then
                                -- make Pair a b = ∀c. (a => b => c) => c (see TAPL pg 352)
  | Datatype Pos TypeMap
  -- Session Types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity Type
  | Choice Pos Polarity TypeMap
  -- Type Variable
  | TypeVar Pos TypeVar  
  -- Polymorphism
  | Forall Pos KindBind Type    -- ∀ a:k => T
  -- Recursive Types
  | Rec Pos KindBind Type       -- μ a:k => T
  -- Type operators
  | TAbs Pos KindBind Type      -- λ a:k => T -- TODO: Rename to Abs
  | TApp Pos Type Type                        -- TODO: Rename to App
  | Dualof Pos Type             -- TODO: eliminate
  -- Named Type, to be looked upon in a map of type names to types, tEnv
  | TypeName Pos TypeVar 
  deriving (Eq, Ord)

type TypeMap = Map.Map ProgVar Type

instance Position Type where
  -- Functional types
  position (IntType p)      = p
  position (CharType p)     = p
  position (BoolType p)     = p
  position (UnitType p)     = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  -- Session types
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  -- Polymorphism
  position (Forall p _ _)   = p
  -- Functional or session
  position (Rec p _ _)      = p
  position (TypeVar p _)    = p
  -- Type operators
  position (Dualof p _)     = p
  position (TypeName p _)   = p

instance Default Type where
  omission p = IntType p

-- Binding program variables to types

data TypeBind = TypeBind Pos ProgVar Type deriving (Eq, Ord)

instance Position TypeBind where
  position (TypeBind p _ _) = p

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
equalTypes s (Rec _ (KindBind _ x k) t) (Rec _ (KindBind _ y l) u) =
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


-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (Kind, Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar Type
