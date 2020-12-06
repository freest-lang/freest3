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
  )
where

import           Control.Monad                  ( liftM2 )
import           Syntax.Base
import qualified Syntax.Type                   as T
import           Utils.FreestState

-- The dual function on types, etc
-- class Dual t where
--   dual :: t -> t

-- instance Dual Polarity where
dualPol :: T.Polarity -> T.Polarity
dualPol T.In  = T.Out
dualPol T.Out = T.In

-- instance Dual Type where
dual :: T.Type -> FreestState T.Type
dual t@T.Skip{}          = pure t
dual (T.Semi    p t1 t2) = liftM2 (T.Semi p) (dual t1) (dual t2)
dual (T.Message p v  b ) = pure $ T.Message p (dualPol v) b
dual (T.Choice  p v  m ) = fmap (T.Choice p (dualPol v)) (mapM dual m)
dual (T.Rec     p x  t ) = fmap (T.Rec p x) (dual t)
dual t@T.TypeVar{}       = pure t
dual t                   = do
  addError (pos t) [Error "Dualof applied to a non session type: ", Error t]
  pure t
-- dual t = internalError (pos t) "Validation.Duality.dual" (show t)
