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
( KindVar
, PreKind (..)
, Multiplicity (..)
, Kind (..)
, KindEnv
, top
, lub
) where

import           Parse.Lexer (Position, Pos, position, defaultPos)
import           Syntax.Bind (Var, Bind)
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
  (Kind _ p n) == (Kind _ q m) = (p, n) == (q, n)

instance Ord Kind where
  (Kind _ Session    Un)  <= _                       = True
  (Kind _ Functional Un)  <= (Kind _ Functional Un)  = True
  (Kind _ Functional Un)  <= (Kind _ Functional Lin) = True
  (Kind _ Session    Lin) <= (Kind _ Session    Lin) = True
  (Kind _ Session    Lin) <= (Kind _ Functional Lin) = True
  (Kind _ Functional Lin) <= (Kind _ Functional Lin) = True
  _                       <= _                       = False  

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

instance Position Kind where
  position (Kind p _ _) = p

-- The least upper bound of two kinds
lub :: Kind -> Kind -> Kind
lub (Kind p Functional Un) (Kind _ Functional Lin) = top p
lub k1 k2 = max k1 k2

-- The kind that seats at the top of the hierarchy (use as a default value)
top :: Pos -> Kind
top p = Kind p Functional Lin

-- KIND VAR

type KindVar = Var -- = String

-- KIND ENVIRONMENT

type KindEnv = Map.Map Bind Kind

