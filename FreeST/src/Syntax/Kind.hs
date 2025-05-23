{- |
Module      :  Syntax.Kind
Description :  The kind of a type
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines kinds. It also defines the subkinding relation, the least
upper bound of two kinds and other functions to manipulate kinds.
-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Kind
  ( PreKind(..)
  , Kind(..)
  , Multiplicity(..)
  , KindEnv
  , PolyVars
  , lt
  , ut
  , ls
  , us
  , la
  , ua
  , isLin
  , isUn
  , isSession
  , prekind
  , mult
  )
where

import           Syntax.Base -- hiding ( Multiplicity(..) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data PreKind = Session | Top | Absorb | PKVar Variable deriving (Ord, Eq)

data Kind = Kind Span Multiplicity PreKind

instance Eq Kind where
  (Kind _ m1 b1) == (Kind _ m2 b2) = m1 == m2 && b1 == b2

instance Ord Kind where
  (Kind _ m1 pk1) <= (Kind _ m2 pk2) = m1 <= m2 && pk1 <= pk2

instance Located Kind where
  getSpan (Kind p _ _) = p

-- The kind of conventional (non linear, non session) functional programming
-- languages' types. Alternative: the kind that sits at the top of the
-- hierarchy
instance Default Kind where
  omission _ = ut defaultSpan

-- Get prekind and mult from a kind
prekind :: Kind -> PreKind
prekind (Kind _ _ v) = v

mult :: Kind -> Multiplicity
mult (Kind _ m _) = m

-- Abbreviations for the six available kinds
lt, ut, ls, us, la, ua :: Span -> Kind
lt p = Kind p Lin Top 
ut p = Kind p Un  Top 
ls p = Kind p Lin Session 
us p = Kind p Un  Session
la p = Kind p Lin Absorb
ua p = Kind p Un  Absorb

isLin :: Kind -> Bool
isLin (Kind _ MultVar{} _) = False
isLin (Kind _ m _) = m == Lin

isUn :: Kind -> Bool
isUn (Kind _ MultVar{} _) = False
isUn k = not $ isLin k

isSession :: Kind -> Bool
isSession (Kind _ _ b) = b == Session

-- Kind environment

type KindEnv = Map.Map Variable Kind

type PolyVars = Set.Set Variable

instance (Default a) => Default (Bind Kind a) where
  omission p = Bind p (omission p) (omission p) (omission p)
