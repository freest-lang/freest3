
{- |
Module      :  Syntax.Kind
Description :  The kind of a type
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a kind and the relational order between kinds. It also defines the
subkinding relation, the least upper bound of two kinds and other functions to
manipulate prekinds and multiplicities.
-}

module Syntax.Kind
  ( PreKind(..)
  , Bind(..)
  , Kind(..)
  , KindEnv
  , kindTL
  , kindTU
  , kindSL
  , kindSU
  , kindMU
  , kindML
  , isSession
  , (<:)
  , join
  , isLin
  , isUn
  )
where

import           Syntax.TypeVariable
import           Syntax.Base
import qualified Data.Map.Strict               as Map

-- Prekinds

data PreKind = Message | Session | Functional deriving Eq

instance Ord PreKind where
  Session <= Functional = True
  _       <= _          = False

-- Kinds

data Kind = Kind Pos PreKind Multiplicity
          deriving Ord -- TODO: I wish we do not need this

instance Eq Kind where
  (Kind _ p n) == (Kind _ q m) = p == q && n == m

-- Abbreviations for the four kinds
kindTL, kindTU, kindSL, kindSU, kindMU, kindML :: Pos -> Kind
kindTL p = Kind p Functional Lin
kindTU p = Kind p Functional Un
kindSL p = Kind p Session Lin
kindSU p = Kind p Session Un
kindMU p = Kind p Message Un
kindML p = Kind p Message Lin

-- The subkinding relation. Note that Kind is a partial order, hence
-- should *not* be an instance class Ord.
--      TL
--    / | \
--   ML TU SL
--   \ / \ /
--    MU  SU

(<:) :: Kind -> Kind -> Bool
(Kind _ Session m1) <: (Kind _ Functional m2) = m1 <= m2
(Kind _ Message m1) <: (Kind _ Functional m2) = m1 <= m2
(Kind _ k1      m1) <: (Kind _ k2         m2) = k1 == k2 && m1 <= m2

-- The least upper bound of two kinds
join :: Kind -> Kind -> Kind
join (Kind p Functional Un ) (Kind _ Session    Lin) = kindTL p
join (Kind p Session    Lin) (Kind _ Functional Un ) = kindTL p
join (Kind p Functional Un ) (Kind _ Message    Lin) = kindTL p
join (Kind p Message    Lin) (Kind _ Functional Un ) = kindTL p
join k1 k2 = if k1 <: k2 then k2 else k1

-- The kind of conventional (non linear, not sessions) functional
-- programming languages (Alternative: the kind that sits at the top
-- of the hierarchy)
instance Default Kind where
  omission = kindTU

isSession :: Kind -> Bool
isSession = (<: kindSL defaultPos)

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
