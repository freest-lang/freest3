{- |
Module      :  Validation.Duality
Description :  The duality module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a types, duality and equality.
-}

module Validation.Duality
( dual -- Dual(..)
) where

import           Syntax.Types
import           Syntax.Base
import           Utils.FreestState
import           Control.Monad          (liftM2)
import qualified Data.Map.Strict as Map

-- The dual function on types, etc
-- class Dual t where
--   dual :: t -> t

-- instance Dual Polarity where
dualPol  In  = Out
dualPol Out = In

-- instance Dual Type where
dual :: Type -> FreestState Type
dual t@Skip{} = pure t
dual (Semi p t1 t2) = liftM2 (Semi p) (dual t1) (dual t2)
dual (Message p v b) = pure $ Message p (dualPol v) b
dual (Choice p v m) = fmap (Choice p (dualPol v)) (mapM dual m)
dual (Rec p x t) = fmap (Rec p x) (dual t)
dual t@TypeVar{} = pure t
dual t = do
  addError (pos t) [Error "Dualof applied to a non session type: ", Error t]
  pure t
-- dual t = internalError (pos t) "Validation.Duality.dual" (show t)
