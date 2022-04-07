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

-- FIXME: instance Span ???
instance Position Type where
  pos (Int  p       ) = startPos p
  pos (Char p       ) = startPos p
  pos (Bool p       ) = startPos p
  pos (String p     ) = startPos p
  pos (Unit p       ) = startPos p
  pos (Arrow p _ _ _) = startPos p
  pos (Pair p _ _   ) = startPos p
  pos (Almanac p _ _) = startPos p
  pos (Skip p       ) = startPos p
  pos (Semi p _ _   ) = startPos p
  pos (Message p _ _) = startPos p
  pos (Forall p _   ) = startPos p
  pos (Rec p _      ) = startPos p
  pos (Var p _      ) = startPos p
  -- pos (Abs p _      ) = startPos p
  -- pos (App p _ _    ) = startPos p
  pos (Dualof p _   ) = startPos p
  pos (CoVar p _   ) = startPos p

-- FIXME: change Default ???
instance Default Type where
  omission = Int

instance Spannable Type where
  span (Int  p       ) = p
  span (Char p       ) = p
  span (Bool p       ) = p
  span (String p     ) = p
  span (Unit p       ) = p
  span (Arrow p _ _ _) = p
  span (Pair p _ _   ) = p
  span (Almanac p _ _) = p
  span (Skip p       ) = p
  span (Semi p _ _   ) = p
  span (Message p _ _) = p
  span (Forall p _   ) = p
  span (Rec p _      ) = p
  span (Var p _      ) = p
  -- span (Abs p _      ) = p
  -- span (App p _ _    ) = p
  span (Dualof p _   ) = p
  span (CoVar p _   ) = p
