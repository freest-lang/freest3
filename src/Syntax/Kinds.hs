{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Kinds
( PreKind (..)
, Multiplicity (..)
, Kind (..)
, KindEnv
, topUn
, isSession
, (<:)
, lub
, isLin
, isUn
) where

import           Parse.Lexer (Position, Pos, position, defaultPos)
import           Syntax.Bind
import qualified Data.Map.Strict as Map

-- PREKINDS

data PreKind = Session | Functional deriving Eq

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

instance Ord PreKind where
   Session <= Functional = True
   _       <= _          = False

-- MULTIPLICITIES

data Multiplicity = Un | Lin deriving Eq

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

instance Ord Multiplicity where
  Un <= Lin = True
  _  <= _  = False

-- KINDS

data Kind = Kind Pos PreKind Multiplicity

instance Eq Kind where
  (Kind _ p n) == (Kind _ q m) = (p, n) == (q, m)

-- The subkinding relation. Note that Kind is a partial order, hence
-- should not implement class Ord.
--    TL
--   /  \
-- TU    SL
--   \  /
--    SU
(<:) :: Kind -> Kind -> Bool
(Kind _ Session Un)    <: _                       = True
(Kind _ Session Lin)   <: (Kind _ Functional Lin) = True
(Kind _ Functional Un) <: (Kind _ Functional Lin) = True
k1                     <: k2                      = k1 == k2

-- The least upper bound of two kinds
lub :: Kind -> Kind -> Kind
lub (Kind p Functional Un)  (Kind _ Session    Lin) = omission p
lub (Kind p Session    Lin) (Kind _ Functional Un)  = omission p
lub k1                      k2                      = if k1 <: k2 then k2 else k1

-- The kind that seats at the top of the hierarchy (use as a default value)
instance Default Kind where
  omission p = Kind p Functional Lin

topUn :: Pos -> Kind
topUn p = Kind p Functional Un

isSession :: Kind -> Bool
isSession = (<: (Kind defaultPos Session Lin))

isLin :: Kind -> Bool
isLin (Kind _ _ Lin) = True
isLin _              = False

isUn :: Kind -> Bool
isUn = not . isLin

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

instance Position Kind where
  position (Kind p _ _) = p

-- KIND ENVIRONMENTS

type KindEnv = Map.Map TBind Kind

