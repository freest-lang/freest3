-- TODO: header here

module Types.Kinds
( PreKind (..)
, Multiplicity (..)
, Kind (..)
) where

  

data PreKind = Session | Functional  deriving (Eq, Ord, Read)

data Multiplicity = Un | Lin deriving (Eq, Ord, Read)

-- TODO: Turn into a record
data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Read)

-- newtype Kind = Kind {getPreKindMultiplicity :: (PreKind, Multiplicity)}

instance Show Kind where
  show (Kind Session Lin) = "SL"
  show (Kind Session Un) = "SU"
  show (Kind Functional Lin) = "TL"
  show (Kind Functional Un) = "TU" 
