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
  ( Basic(..)
  , Kind(..)
  , Multiplicity(..)
  , KindEnv
  , PolyVars
  , tl
  , tu
  , sl
  , su
  , mu
  , ml
  , isLin
  , isUn
  , isSession
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Syntax.Base             hiding ( Multiplicity(..) )
-- Basic kind

data Basic = Message | Session | Top deriving Eq

-- Multiplicity
data Multiplicity = Un | Lin deriving Eq

-- Kind

data Kind = Kind Pos Basic Multiplicity

instance Position Kind where
  pos (Kind p _ _) = p

-- The kind of conventional (non linear, non session) functional programming
-- languages' types (Alternative: the kind that sits at the top of the
-- hierarchy)
instance Default Kind where
  omission = tu

-- Abbreviations for the six proper kinds
tl, tu, sl, su, mu, ml :: Pos -> Kind
tl p = Kind p Top Lin
tu p = Kind p Top Un
sl p = Kind p Session Lin
su p = Kind p Session Un
mu p = Kind p Message Un
ml p = Kind p Message Lin

isLin :: Kind -> Bool
isLin (Kind _ _ m) = m == Lin

isUn :: Kind -> Bool
isUn = not . isLin

isSession :: Kind -> Bool
isSession (Kind _ b _) = b == Session

-- Kind environment

type KindEnv = Map.Map Variable Kind

type PolyVars = Set.Set Variable

instance (Default a) => Default (Bind Kind a) where
  omission p = Bind p (omission p) (omission p) (omission p)