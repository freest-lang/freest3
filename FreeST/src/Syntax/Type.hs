{- |
Module      :  Syntax.Types
Description :  The language types.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types and equality.
-}

module Syntax.Type
  ( -- BasicType(..),
    Type(..)
  , Bind(..)
  , TypeMap
  , Polarity(..)
  , TypeOpsEnv
  , TypeEnv
  , VarEnv
  )
where

import qualified Syntax.Kind                   as K
import           Syntax.TypeVariable
import           Syntax.ProgramVariable         ( ProgVar )
import           Syntax.Base
import qualified Data.Map.Strict               as Map

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
  | Pair Pos Type Type        -- make Pair a b = ∀c. (a => b => c) => c (see TAPL pg 352)
  | Datatype Pos TypeMap
  -- Session Types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity Type
  | Choice Pos Polarity TypeMap
  -- Type Variable
  | TypeVar Pos TypeVar
  -- Polymorphism
  | Forall Pos K.Bind Type    -- ∀ a:k => T
  -- Recursive Types
  | Rec Pos K.Bind Type       -- μ a:k => T
  -- Type operators
  | Abs Pos K.Bind Type      -- λ a:k => T
  | App Pos Type Type
  | Dualof Pos Type             -- TODO: eliminate
  -- Named Type, to be looked upon in a map of type names to types, tEnv
  | TypeName Pos TypeVar
  deriving (Eq, Ord)

type TypeMap = Map.Map ProgVar Type

instance Position Type where
  -- Functional types
  pos (IntType  p   ) = p
  pos (CharType p   ) = p
  pos (BoolType p   ) = p
  pos (UnitType p   ) = p
  pos (Fun p _ _ _  ) = p
  pos (Pair p _ _   ) = p
  pos (Datatype p _ ) = p
  -- Session types
  pos (Skip p       ) = p
  pos (Semi    p _ _) = p
  pos (Message p _ _) = p
  pos (Choice  p _ _) = p
  -- Polymorphism
  pos (Forall  p _ _) = p
  -- Functional or session
  pos (Rec     p _ _) = p
  pos (TypeVar p _  ) = p
  -- Type operators
  pos (Abs p _ _    ) = p
  pos (App p _ _    ) = p
  pos (Dualof   p _ ) = p
  pos (TypeName p _ ) = p

instance Default Type where
  omission = IntType

-- Binding program variables to types

data Bind = Bind Pos ProgVar Type deriving (Eq, Ord)

instance Position Bind where
  pos (Bind p _ _) = p

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
equalTypes s (Rec _ (Bind _ x k) t) (Rec _ (Bind _ y l) u) =
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
type TypeEnv = Map.Map TypeVar (K.Kind, Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar Type
