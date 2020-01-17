{- |
Module      :  Syntax.Duality
Description :  The duality module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types, duality and equality.
-}

module Syntax.Duality
( Dual(..)
) where

import Syntax.Types
import qualified Data.Map.Strict as Map

-- The dual function on types, etc
class Dual t where
  dual :: t -> t

instance Dual Polarity where
  dual In  = Out
  dual Out = In

instance Dual Type where
  -- Session types
  dual (Semi p t1 t2)   = Semi p (dual t1) (dual t2)
  -- dual (Semi p t1 t2)  = Semi p (Dualof p t1) (Dualof p t2) -- The lazy version loops
  dual (Message p v b)  = Message p (dual v) b
  dual (Choice p v m)   = Choice p (dual v) (Map.map dual m)
  -- dual (Choice p v m)  = Choice p (dual v) (Map.map (Dualof p) m) -- The lazy version loops
  dual (Rec p x t)      = Rec p x (dual t)
  -- dual (Rec p x t)     = Rec p x (Dualof p t) -- The lazy version loops
  -- Type operators
  dual (Dualof _ t)     = t
  dual t@(TypeName _ x) = t -- TODO: This can't be right
  -- Functional types, Skip, TypeVar
  dual t                = t
