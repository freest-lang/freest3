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

module Syntax.Kind
  ( Basic(..)
  , Bind(..)
  , Kind(..)
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
  , body
  )
where

import           Syntax.TypeVariable
import           Syntax.Base
import qualified Data.Map.Strict               as Map
import qualified Data.Set as Set

-- Basic kind

data Basic = Message | Session | Top deriving Eq

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

-- Bind: ∀ a:k . t or Λ a:k => e

data Bind a = Bind Pos TypeVar Kind a

instance Position (Bind a) where
  pos (Bind p _ _ _) = p

instance Default a => Default (Bind a) where
  omission p = Bind p (omission p) (omission p) (omission p)

body :: Bind a -> a
body (Bind _ _ _ a) = a

-- Kind environment

type KindEnv = Map.Map TypeVar Kind

type PolyVars = Set.Set TypeVar
