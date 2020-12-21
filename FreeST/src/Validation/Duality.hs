{- |
Module      :  Validation.Duality
Description :  Session type duality.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

The dual function on session types
-}

module Validation.Duality
  ( Dual(..)
  )
where

import           Control.Monad                  ( liftM2 )
import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Utils.FreestState

class Dual t where
  dual :: t -> FreestState t

instance Dual T.Polarity where
  dual T.In  = pure T.Out
  dual T.Out = pure T.In

instance Dual t => Dual (K.Bind t) where
  dual (K.Bind p a k t) = fmap (K.Bind p a k) (dual t)

instance Dual T.Type where
  dual :: T.Type -> FreestState T.Type
  dual t@T.Skip{}        = pure t
  dual (T.Semi    p t u) = liftM2 (T.Semi p) (dual t) (dual u)
  dual (T.Message p v t) = pure $ T.Message p (dualPol v) t
  dual (T.Choice  p v m) = fmap (T.Choice p (dualPol v)) (mapM dual m)
  dual (T.Rec     p b  ) = fmap (T.Rec p) (dual b)
  dual t@T.Var{}         = pure t
  dual t                 = do
    addError (pos t) [Error "Dualof applied to a non session type: ", Error t]
    pure t

dualPol :: T.Polarity -> T.Polarity
dualPol T.In  = T.Out
dualPol T.Out = T.In
