-- TODO: header here

module Types.Kinds
( PreKind (..)
, Multiplicity (..)
, Kind (..)
) where

-- TODO: Arbitrary -> Functional
-- TODO: Remove Scheme
data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show, Read)

data Multiplicity = Un | Lin deriving (Eq, Ord, Show, Read)

-- TODO: Turn into a record
data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show, Read)
