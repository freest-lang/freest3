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
  , Level(..)
  , unit 
  , tuple 
--  , Multiplicity(..)
  , free
  , isFreeIn
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K
import           Syntax.MkName (mkTupleLabels)
import qualified Data.Map.Strict               as Map
import qualified Data.Set as Set

data Polarity = Out | In deriving (Eq, Ord)

data View = External | Internal deriving (Eq, Ord)

data Sort = Record | Variant | Choice View deriving (Eq, Ord)

data Level = Top | Bottom | Num Int deriving (Eq, Ord)

data Type =
  -- Functional Types
    Int Span
  | Float Span
  | Char Span
  | String Span
  | Arrow Span Multiplicity Level Level Type Type
  | Labelled Span Sort Level TypeMap
  -- Session Types
  | Skip Span
  | End Span Polarity Level 
  | Semi Span Type Type
  | Message Span Level Polarity Type
  -- Polymorphism and recursive types
  | Forall Span (Bind K.Kind Type)   -- ∀k . T, Universal type
  | Rec Span (Bind K.Kind Type)      -- μ a:k . T, Recursive type
  | Var Span Variable
  -- Type operators
  | Dualof Span Type

-- | Abs Pos (Bind Type)       -- λ a:k => T, Operator abstraction
-- | App Pos Type Type

type TypeMap = Map.Map Variable Type

instance Default Type where
  omission = Int

instance Located Type where
  getSpan (Int  p        ) = p
  getSpan (Float p       ) = p
  getSpan (Char p        ) = p
  getSpan (String p      ) = p
  getSpan (Arrow p _ _ _ _ _ ) = p
  getSpan (Labelled p _ _ _) = p
  getSpan (Skip p        ) = p
  getSpan (End p _ _     ) = p
  getSpan (Semi p _ _    ) = p
  getSpan (Message p _ _ _ ) = p
  getSpan (Forall p _    ) = p
  getSpan (Rec p _       ) = p
  getSpan (Var p _       ) = p
  getSpan (Dualof p _    ) = p

-- Derived forms
tuple :: Span -> [Type] -> Type
tuple s ts = Labelled s Record Bottom tupleTypeMap
  where tupleTypeMap =
          Map.fromList $ zipWith (\mk t -> (mk (getSpan t), t)) mkTupleLabels ts 

unit :: Span -> Type 
unit s = tuple s []

-- The set of free type variables in a type
free :: Type -> Set.Set Variable
  -- Functional Types
free (Arrow _ _ _ _ t u) = free t `Set.union` free u
free (Labelled _ _ _ m) =
  Map.foldr (\t acc -> free t `Set.union` acc) Set.empty m
  -- Session Types
free (Semi _ t u) = free t `Set.union` free u
free (Message _ _ _ t) = free t 
  -- Polymorphism and recursive types
free (Forall _ (Bind _ a _ t)) = Set.delete a (free t)
free (Rec _ (Bind _ a _ t)) = Set.delete a (free t)
free (Var _ x) = Set.singleton x
  -- Type operators
free (Dualof _ t) = free t
  --Int, Float, Char, String, Skip, End
free _ = Set.empty 

isFreeIn :: Variable -> Type -> Bool
isFreeIn a t = a `Set.member` free t

-- Does a given type variable x occur free in a type t?
-- If not, then rec x.t can be renamed to t alone.
-- isFreeIn :: Variable -> T.Type -> Bool
--   -- Labelled
-- isFreeIn x (T.Labelled _ _ m) =
--   Map.foldr' (\t b -> x `isFreeIn` t || b) False m
--     -- Functional types
-- isFreeIn x (T.Arrow _ _ t u) = x `isFreeIn` t || x `isFreeIn` u
--     -- Session types
-- isFreeIn x (T.Message _ _ t) = x `isFreeIn` t ---
-- isFreeIn x (T.Semi _ t u) = x `isFreeIn` t || x `isFreeIn` u
--   -- Polymorphism
-- isFreeIn x (T.Forall _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
--   -- Functional or session 
-- isFreeIn x (T.Rec    _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
-- isFreeIn x (T.Var    _ y               ) = x == y
--   -- Type operators
-- isFreeIn x (T.Dualof _ t) = x `isFreeIn` t
-- isFreeIn _ _                             = False
