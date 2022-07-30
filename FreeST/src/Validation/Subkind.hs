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
  )
where

import qualified Syntax.Kind                   as K
import           Util.Error                    ( internalError )

-- The subkinding relation. Note that subkinding is a partial order, hence
-- should *not* be an instance class Ord.
--      TL
--     /  \
--    TU  SL
--     \ /
--      SU

-- The Subsort class. Instances include multiplicity, basic kinds and kinds

class Subsort t where
  (<:) :: t -> t -> Bool

instance Subsort K.Multiplicity where
  K.Lin <: K.Un = False
  _     <: _  = True

instance Subsort K.Basic where
  K.Session <: K.Top     = True
  K.Session <: K.Session = True
  K.Top     <: K.Top     = True
  _         <: _         = False

instance Subsort K.Kind where
  (K.Kind _ b1 m1) <: (K.Kind _ b2 m2) = b1 <: b2 && m1 <: m2

-- The least upper bound of two kinds

class Join t where
  join :: t -> t -> t

instance Join K.Multiplicity where
  join K.Un  K.Lin = K.Lin
  join K.Lin K.Un  = K.Lin
  join K.Un  K.Un  = K.Un
  join K.Lin K.Lin = K.Lin

instance Join K.Kind  where
  join (K.Kind p K.Un  K.Top    ) (K.Kind _ K.Lin K.Session) = K.lt p
  join (K.Kind p K.Lin K.Session) (K.Kind _ K.Un  K.Top    ) = K.lt p


  join k1 k2
    | k1 <: k2 = k2
    | k2 <: k1 = k1
    | otherwise = internalError "Validation.Subkind.join" k1
  
