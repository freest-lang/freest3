{-# LANGUAGE FlexibleInstances #-}
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
    Int Span
  | Char Span
  | Bool Span
  | String Span
  | Unit Span
  | Arrow Span Multiplicity Type Type
  | Pair Span Type Type
  | Almanac Span Sort TypeMap
  -- Session Types
  | Skip Span
  | End Span
  | Semi Span Type Type
  | Message Span Polarity Type
  -- Polymorphism and recursive types
  | Forall Span (Bind K.Kind Type)   -- ∀k . T, Universal type
  | Rec Span (Bind K.Kind Type)      -- μ a:k . T, Recursive type
  | Var Span Variable
  -- Type operators
  | Dualof Span Type
  | CoVar Span Variable

-- | Abs Pos (Bind Type)       -- λ a:k => T, Operator abstraction
-- | App Pos Type Type

type TypeMap = Map.Map Variable Type

data Sort = Record | Variant | Choice View deriving Eq

instance Default Type where
  omission = Int

instance Located Type where
  getSpan (Int  p       ) = p
  getSpan (Char p       ) = p
  getSpan (Bool p       ) = p
  getSpan (String p     ) = p
  getSpan (Unit p       ) = p
  getSpan (Arrow p _ _ _) = p
  getSpan (Pair p _ _   ) = p
  getSpan (Almanac p _ _) = p
  getSpan (Skip p       ) = p
  getSpan (End p        ) = p
  getSpan (Semi p _ _   ) = p
  getSpan (Message p _ _) = p
  getSpan (Forall p _   ) = p
  getSpan (Rec p _      ) = p
  getSpan (Var p _      ) = p
  -- getSpan (Abs p _      ) = p
  -- getSpan (App p _ _    ) = p
  getSpan (Dualof p _   ) = p
  getSpan (CoVar p _   ) = p
