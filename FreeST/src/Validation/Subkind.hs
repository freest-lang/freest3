{- |
Module      :  Validation.Subkind
Description :  The subkind relation
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

-}
{-# LANGUAGE FlexibleInstances #-}

module Validation.Subkind
  ( Subsort(..)
  , join
  )
where

import qualified Syntax.Kind                   as K
import qualified Syntax.Type as T
import Syntax.Program (VarEnv)
import qualified Data.Map.Strict as Map

-- The subkinding relation. Note that subkinding is a partial order, hence
-- should *not* be an instance class Ord.
--      1T
--     /  \
--    *T  1S
--     \ /
--      *S

-- The Subsort class. Instances include Multiplicity, Basic kinds and Kind

class Subsort t where
  (<:) :: t -> t -> Bool

instance Subsort K.Multiplicity where
  K.Lin <: K.Un = False
  _     <: _    = True

instance Subsort K.Basic where
  K.Top <: K.Session = False
  _     <: _         = True

instance Subsort K.Kind where
  (K.Kind _ b1 m1) <: (K.Kind _ b2 m2) = b1 <: b2 && m1 <: m2

-- The least upper bound of two kinds

class Join t where
  join :: t -> t -> t

instance Join K.Multiplicity where
  join K.Un K.Un = K.Un
  join _    _    = K.Lin

instance Join K.Basic where
  join K.Session K.Session = K.Session
  join _         _         = K.Top

instance Join K.Kind where
  join (K.Kind span m1 b1) (K.Kind _ m2 b2) = K.Kind span (join m1 m2) (join b1 b2)
