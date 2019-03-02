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
  
data PreKind = Session | Functional deriving (Eq)

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

data Multiplicity = Un | Lin deriving Eq

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

data Kind = Kind {prekind :: PreKind, multiplicity :: Multiplicity}
  deriving (Eq, Ord)

instance Show Kind where
  show k = show (prekind k) ++ show (multiplicity k)

instance Ord Multiplicity where
  Lin <= Un = False
  _   <= _  = True

instance Ord PreKind where
   Functional <= Session = False
   _          <= _       = False
