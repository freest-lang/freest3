{- |
Module      :  Syntax.Duality
Description :  The duality module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types, duality and equality.
-}

module Validation.Duality
( Dual(..)
) where

import           Syntax.Types
import           Syntax.Show
import qualified Data.Map.Strict as Map

-- The dual function on types, etc
class Dual t where
  dual :: t -> t

instance Dual Polarity where
  dual In  = Out
  dual Out = In

instance Dual Type where
  -- Session types
  dual t@(Skip _)      = t
  dual (Semi p t1 t2)  = Semi p (dual t1) (dual t2)
  dual (Message p v b) = Message p (dual v) b
  dual (Choice p v m)  = Choice p (dual v) (Map.map dual m)
  dual (Rec p x t)     = Rec p x (dual t)
  dual t@(TypeVar _ _) = t
  -- Type operators
  -- dual (Dualof _ t)     = t
  -- dual t@(TypeName _ x) = t -- TODO: This can't be right
  -- Functional types, Skip, TypeVar
  -- dual t                = t
  dual t = error $ "Internal error: Validation.dual called on type " ++ show t
