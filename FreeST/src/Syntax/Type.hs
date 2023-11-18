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
  , unit 
  , tuple 
--  , Multiplicity(..)
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Data.Map.Strict               as Map
import Syntax.MkName (mkTupleLabels)

data Polarity = Out | In deriving Eq
data View = External | Internal deriving Eq

data Type =
  -- Functional Types
    Int (Span Type)
  | Float (Span Type)
  | Char (Span Type)
  | String (Span Type)
  | Arrow (Span Type) Multiplicity Type Type
  | Labelled (Span Type) Sort TypeMap
  -- Session Types
  | Skip (Span Type)
  | End (Span Type)
  | Semi (Span Type) Type Type
  | Message (Span Type) Polarity Type
  -- Polymorphism and recursive types
  | Forall (Span Type) (Bind K.Kind Type)   -- ∀k . T, Universal type
  | Rec (Span Type) (Bind K.Kind Type)      -- μ a:k . T, Recursive type
  | Var (Span Type) Variable
  -- Type operators
  | Dualof (Span Type) Type
--  | CoVar Span Variable

-- | Abs Pos (Bind Type)       -- λ a:k => T, Operator abstraction
-- | App Pos Type Type

type TypeMap = Map.Map Variable Type

data Sort = Record | Variant | Choice View deriving Eq

instance Default Type where
  omission s = Int (clear s)

instance Located Type where
  getSpan (Int  p       ) = p
  getSpan (Float p      ) = p
  getSpan (Char p       ) = p
  getSpan (String p     ) = p
  getSpan (Arrow p _ _ _) = p
  getSpan (Labelled p _ _) = p
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
--  getSpan (CoVar p _   ) = p

-- Derived forms
unit :: Span a -> Type 
unit s = Labelled (clear s) Record Map.empty 

tuple :: Span Type -> [Type] -> Type
tuple s ts = Labelled s Record (tupleTypeMap ts)
  where tupleTypeMap :: [Type] -> TypeMap
        tupleTypeMap ts = Map.fromList $ zipWith (\mk t -> (mk (getSpan t), t)) mkTupleLabels ts 