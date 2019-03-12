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
) where

import           Syntax.Position
import qualified Data.Map.Strict as Map

-- PREKINDS

data PreKind = Session | Functional deriving Eq

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

instance Ord PreKind where
   Session <= Functional = True
   _          <= _       = False

-- MULTIPLICITIES

data Multiplicity = Un | Lin deriving Eq

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

instance Ord Multiplicity where
  Un <= Lin = True
  _  <= _  = False

-- KINDS

data Kind = Kind {prekind :: PreKind, multiplicity :: Multiplicity} -- TOPO: include position; use a triple.
  deriving (Eq, Ord)

instance Show Kind where
  show k = show (prekind k) ++ show (multiplicity k)
