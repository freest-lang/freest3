{- |
Module      :  Validation.Subkind
Description :  The subkind relation
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

-}

module Validation.Subkind
  ( (<:)
  , join
  , isSession
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K

-- The subkinding relation. Note that subkinding is a partial order, hence
-- should *not* be an instance class Ord.
--      TL
--    / | \
--   ML TU SL
--   \ / \ /
--    MU  SU

-- The Subsort class. Instances include multiplicity, basic kinds and kinds

class Subsort t where
  (<:) :: t -> t -> Bool

instance Subsort Multiplicity where
  Lin <: Un = False
  _   <: _  = True

instance Subsort K.Basic where
  K.Message <: K.Top     = True
  K.Session <: K.Top     = True
  K.Message <: K.Message = True
  K.Session <: K.Session = True
  K.Top     <: K.Top     = True
  _         <: _         = False

instance Subsort K.Kind where
  (K.Kind _ b1 m1) <: (K.Kind _ b2 m2) = b1 <: b2 && m1 <: m2

-- The least upper bound of two kinds
join :: K.Kind -> K.Kind -> K.Kind
join (K.Kind p K.Message Lin) (K.Kind _ K.Top     Un ) = K.tl p
join (K.Kind p K.Top     Un ) (K.Kind _ K.Message Lin) = K.tl p

join (K.Kind p K.Top     Un ) (K.Kind _ K.Session Lin) = K.tl p
join (K.Kind p K.Session Lin) (K.Kind _ K.Top     Un ) = K.tl p

join (K.Kind p K.Message Un ) (K.Kind _ K.Session Un ) = K.tu p
join (K.Kind p K.Session Un ) (K.Kind _ K.Message Un ) = K.tu p

join (K.Kind p K.Message Un ) (K.Kind _ K.Session Lin) = K.tl p
join (K.Kind p K.Session Lin) (K.Kind _ K.Message Un ) = K.tl p

join (K.Kind p K.Session Un ) (K.Kind _ K.Message Lin) = K.tl p
join (K.Kind p K.Message Lin) (K.Kind _ K.Session Un ) = K.tl p

join k1 k2
  | k1 <: k2 = k2
  | k2 <: k1 = k1
  | otherwise = error "join"

isSession :: K.Kind -> Bool
isSession = (<: K.sl defaultPos)
