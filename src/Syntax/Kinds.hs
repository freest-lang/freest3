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
) where

import Parse.Lexer (Position(..), Pos)
import Syntax.Position

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
  (Kind _ p n) >  (Kind _ q m) = (p, n) >  (q, n)
  (Kind _ p n) <= (Kind _ q m) = (p, n) <= (q, n)

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

instance Position Kind where
  position (Kind p _ _) = p

-- KIND VAR

type KindVar = Var -- = String
