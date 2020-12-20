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
  | Forall Pos K.Bind Type    -- ∀ a:k => T, Universal type
  | Rec Pos K.Bind Type       -- μ a:k => T, Recursive type
  | Var Pos TypeVar
  -- Type operators
  -- | Abs Pos K.Bind Type       -- λ a:k => T, Operator abstraction
  -- | App Pos Type Type
  | Dualof Pos Type
  -- Named Type, to be looked upon in a map of type names to types, tEnv
  | Name Pos TypeVar

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

data Bind = Bind Pos ProgVar Type

instance Position Bind where
  pos (Bind p _ _) = p
