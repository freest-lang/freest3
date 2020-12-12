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
  , tl
  , tu
  , sl
  , su
  , mu
  , ml
  , isLin
  , isUn
  )
where

import           Syntax.TypeVariable
import           Syntax.Base
import qualified Data.Map.Strict               as Map

-- Basic kinds

data Basic = Message | Session | Top
  deriving (Eq, Ord) -- TODO: I wish we wouldn't need this

-- Kinds

data Kind = Kind Pos Basic Multiplicity
  deriving (Eq, Ord) -- TODO: I wish we wouldn't need this

-- Abbreviations for the six proper kinds
tl, tu, sl, su, mu, ml :: Pos -> Kind
tl p = Kind p Top Lin
tu p = Kind p Top Un
sl p = Kind p Session Lin
su p = Kind p Session Un
mu p = Kind p Message Un
ml p = Kind p Message Lin

-- The kind of conventional (non linear, not sessions) functional
-- programming languages (Alternative: the kind that sits at the top
-- of the hierarchy)
instance Default Kind where
  omission = tu

isLin :: Kind -> Bool
isLin (Kind _ _ m) = m == Lin

isUn :: Kind -> Bool
isUn = not . isLin

instance Position Kind where
  pos (Kind p _ _) = p

-- Kind environments

type KindEnv = Map.Map TypeVar Kind

-- Binding type variables to kinds

data Bind = Bind Pos TypeVar Kind deriving (Eq, Ord)

instance Position Bind where
  pos (Bind p _ _) = p
