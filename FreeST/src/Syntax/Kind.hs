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
  )
where

import           Syntax.Base hiding ( Multiplicity(..) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Pre-kind
data PreKind = Session | Top | Absorb deriving (Ord, Eq)

-- Multiplicity
data Multiplicity = Un | Lin deriving Eq

-- Kind
data Kind = Kind (Span Kind) Multiplicity PreKind

instance Eq Kind where
  (Kind _ b1 m1) == (Kind _ b2 m2) = b1 == b2 && m1 == m2

instance Located Kind where
  getSpan (Kind p _ _) = p
  setSpan s (Kind _ m p) = Kind s m p 

-- The kind of conventional (non linear, non session) functional programming
-- languages' types (Alternative: the kind that sits at the top of the
-- hierarchy)
instance Default Kind where
  omission _ = ut defaultSpan

-- Abbreviations for the six proper kinds
lt, ut, ls, us, la, ua :: Span Kind -> Kind
lt p = Kind p Lin Top 
ut p = Kind p Un Top 
ls p = Kind p Lin Session 
us p = Kind p Un Session
la p = Kind p Lin Absorb
ua p = Kind p Un Absorb

isLin :: Kind -> Bool
isLin (Kind _ m _) = m == Lin

isUn :: Kind -> Bool
isUn = not . isLin

isSession :: Kind -> Bool
isSession (Kind _ _ b) = b == Session

-- Kind environment

type KindEnv = Map.Map Variable Kind

type PolyVars = Set.Set Variable

instance (Default a) => Default (Bind Kind a) where
  omission p = Bind p' (omission p') (omission p') (omission p')
    where p' = clearSource p
