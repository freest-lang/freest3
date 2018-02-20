module Types.Kinds
(
  PreKind (..)
, Multiplicity (..)
, Kind (..)
) where

import qualified Data.Map.Strict as Map

data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show, Read)

data Multiplicity = Un | Lin deriving (Eq, Ord, Show, Read)

data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show, Read)
