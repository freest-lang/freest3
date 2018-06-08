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

data PreKind = Session | Functional deriving (Eq, Ord)

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

data Multiplicity = Un | Lin deriving (Eq, Ord, Read)

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

data Kind = Kind {prekind :: PreKind, multiplicity :: Multiplicity}
  deriving (Eq, Ord)

instance Show Kind where
  show k = show (prekind k) ++ show (multiplicity k)

{- Was:

data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Read)

instance Show Kind where
  show (Kind p m) = show p ++ show m
-}


{- Alternative. Worth the refactoring?

newtype Kind = Kind {getPreKindMultiplicity :: (PreKind, Multiplicity)}

instance Show Kind where
  show k = show p ++ show m
    where (p, m) = getPreKindMultiplicity k
-}



