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
  ( Type(..)
  , Bind(..)
  , TypeMap
  , Polarity(..)
  , TypeEnv
  , VarEnv
  , TypeOpsEnv
  , noConstructors
  )
where

import           Syntax.Base
import           Syntax.TypeVariable            ( TypeVar )
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Kind                   as K
import qualified Data.Map.Strict               as Map

data Polarity = In | Out deriving Eq

data Type =
  -- Functional Types
    Int Pos
  | Char Pos
  | Bool Pos
  | Unit Pos
  | Fun Pos Multiplicity Type Type
  | Pair Pos Type Type        -- make Pair a b = ∀c. (a => b => c) => c (see TAPL pg 352)
  | Datatype Pos TypeMap
  -- Session Types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity Type
  | Choice Pos Polarity TypeMap
  -- Polymorphism and recursive types
  | Forall Pos K.Bind Type    -- ∀ a:k => T
  | Rec Pos K.Bind Type       -- μ a:k => T
  | Var Pos TypeVar
  -- Type operators
  -- | Abs Pos K.Bind Type       -- λ a:k => T
  -- | App Pos Type Type
  | Dualof Pos Type
  -- Named Type, to be looked upon in a map of type names to types, tEnv
  | Name Pos TypeVar
  -- deriving (Eq, Ord) -- We use Sets of Types to verify visited types on equivalence. Can we use positions instead?

type TypeMap = Map.Map ProgVar Type

instance Position Type where
  pos (Int  p       ) = p
  pos (Char p       ) = p
  pos (Bool p       ) = p
  pos (Unit p       ) = p
  pos (Fun p _ _ _  ) = p
  pos (Pair p _ _   ) = p
  pos (Datatype p _ ) = p
  pos (Skip p       ) = p
  pos (Semi p _ _   ) = p
  pos (Message p _ _) = p
  pos (Choice p _ _ ) = p
  pos (Forall p _ _ ) = p
  pos (Rec p _ _    ) = p
  pos (Var p _      ) = p
  -- pos (Abs p _ _    ) = p
  -- pos (App p _ _    ) = p
  pos (Dualof p _   ) = p
  pos (Name p _     ) = p

instance Default Type where
  omission = Int

-- Binding program variables to types

data Bind = Bind Pos ProgVar Type -- deriving (Eq, Ord)

instance Position Bind where
  pos (Bind p _ _) = p

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (K.Kind, Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar Type

-- POSITIONS & TYPE OPERATORS (TYPENAME & DUALOF)

type TypeOpsEnv = Map.Map Pos Type

-- A given type environment without constructors
noConstructors :: TypeEnv -> VarEnv -> VarEnv
noConstructors tEnv = Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: ProgVar -> TypeEnv -> Bool
isDatatypeContructor c tEnv =
  not $ Map.null $ Map.filter (isDatatype . snd) tEnv
  where isDatatype :: Type -> Bool
        isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False


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
