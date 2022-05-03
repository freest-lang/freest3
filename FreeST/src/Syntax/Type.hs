{- |
Module      :  Syntax.Types
Description :  The types in FreeST.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the syntax for types.
-}

{-# LANGUAGE FlexibleInstances #-}

module Syntax.Type
  ( TypeOf(..)
  , Type
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

data TypeOf a =
  -- Functional Types
    Int Pos
  | Char Pos
  | Bool Pos
  | String Pos
  | Unit Pos
  | Arrow Pos Multiplicity (TypeOf a) (TypeOf a)
  | Pair Pos (TypeOf a) (TypeOf a)
  | Almanac Pos Sort (TypeMap_ a)
  -- Session Types
  | Skip Pos
  | Semi Pos (TypeOf a) (TypeOf a)
  | Message Pos Polarity (TypeOf a)
  -- Polymorphism and recursive types
  | Forall Pos (Bind K.Kind (TypeOf a))   -- ∀k . T, Universal type
  | Rec Pos (Bind K.Kind (TypeOf a))      -- μ a:k . T, Recursive type
  | Var Pos a
  -- Type_ operators
  | Dualof Pos (TypeOf a)
  | CoVar Pos a

-- | Abs Pos (Bind Type)       -- λ a:k => T, Operator abstraction
-- | App Pos Type Type

type TypeMap_ a = Map.Map Variable (TypeOf a)

type TypeMap = TypeMap_ Variable

type Type = TypeOf Variable

data Sort = Record | Variant | Choice View deriving Eq

{-
instance Eq NamelessType where
  Int _ == Int _ = True
  Char _ == Char _ = True
  Bool _ == Bool _ = True
  String _ == String _ = True
  Unit _ == Unit _ = True
  Arrow _ m1 t11 t12 == Arrow _ m2 t21 t22 = 
    m1 == m2 &&
    t11 == t21 &&
    t12 == t22
  Pair _ t11 t12 == Pair _ t21 t22 =
    t11 == t21 &&
    t12 == t22
  Almanac _ s1 tm1 == Almanac _ s2 tm2 =
    s1 == s2 &&
    tm1 == tm2
  Skip _ == Skip _ = True
  Semi _ t11 t12 == Semi _ t21 t22 =
    t11 == t21 &&
    t12 == t22
  Message _ p1 t1 == Message _ p2 t2 =
    p1 == p2 &&
    t1 == t2
  Forall _ b1 == Forall _ b2 =
    binder b1 == binder b2 &&
    body b1 == body b2
  Rec _ b1 == Rec _ b2 =
    binder b1 == binder b2 &&
    body b1 == body b2
  Var _ i1 == Var _ i2 =
    i1 == i2
  Dualof _ t1 == Dualof _ t2 =
    t1 == t2
  CoVar _ i1 == CoVar _ i2 =
    i1 == i2
  _ == _ = False

instance Eq Type where
  Int _ == Int _ = True
  Char _ == Char _ = True
  Bool _ == Bool _ = True
  String _ == String _ = True
  Unit _ == Unit _ = True
  Arrow _ m1 t11 t12 == Arrow _ m2 t21 t22 = 
    m1 == m2 &&
    t11 == t21 &&
    t12 == t22
  Pair _ t11 t12 == Pair _ t21 t22 =
    t11 == t21 &&
    t12 == t22
  Almanac _ s1 tm1 == Almanac _ s2 tm2 =
    s1 == s2 &&
    tm1 == tm2
  Skip _ == Skip _ = True
  Semi _ t11 t12 == Semi _ t21 t22 =
    t11 == t21 &&
    t12 == t22
  Message _ p1 t1 == Message _ p2 t2 =
    p1 == p2 &&
    t1 == t2
  Forall _ b1 == Forall _ b2 =
    binder b1 == binder b2 &&
    body b1 == body b2
  Rec _ b1 == Rec _ b2 =
    binder b1 == binder b2 &&
    body b1 == body b2
  Var _ i1 == Var _ i2 =
    i1 == i2
  Dualof _ t1 == Dualof _ t2 =
    t1 == t2
  CoVar _ i1 == CoVar _ i2 =
    i1 == i2
  _ == _ = False
-}

instance Position (TypeOf a) where
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

instance Default (TypeOf a) where
  omission = Int
