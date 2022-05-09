{- |
Module      :  Syntax.Types
Description :  The types in FreeST.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the syntax for types.
-}

module Syntax.Type
  ( Type(..)
  , TypeMap
  , Polarity(..)
  , Sort(..)
  , View(..)
--  , Multiplicity(..)
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Data.Map.Strict               as Map

data Polarity = Out | In deriving Eq
data View = External | Internal deriving Eq

data Type =
  -- Functional Types
    Int Pos
  | Char Pos
  | Bool Pos
  | String Pos
  | Unit Pos
  | Arrow Pos Multiplicity Type Type
  | Pair Pos Type Type
  | Almanac Pos Sort TypeMap
  -- Session Types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity Type
  -- Polymorphism and recursive types
  | Forall Pos (Bind K.Kind Type)   -- ∀k . T, Universal type
  | Rec Pos (Bind K.Kind Type)      -- μ a:k . T, Recursive type
  | Var Pos Variable
  -- Type operators
  | Dualof Pos Type
  | CoVar Pos Variable

-- | Abs Pos (Bind Type)       -- λ a:k => T, Operator abstraction
-- | App Pos Type Type

type TypeMap = Map.Map Variable Type

data Sort = Record | Variant | Choice View deriving Eq

instance Position Type where
  pos (Int  p       ) = p
  pos (Char p       ) = p
  pos (Bool p       ) = p
  pos (String p     ) = p
  pos (Unit p       ) = p
  pos (Arrow p _ _ _) = p
  pos (Pair p _ _   ) = p
  pos (Almanac p _ _) = p
  pos (Skip p       ) = p
  pos (Semi p _ _   ) = p
  pos (Message p _ _) = p
  pos (Forall p _   ) = p
  pos (Rec p _      ) = p
  pos (Var p _      ) = p
  -- pos (Abs p _      ) = p
  -- pos (App p _ _    ) = p
  pos (Dualof p _   ) = p
  pos (CoVar p _   ) = p

instance Default Type where
  omission = Int
