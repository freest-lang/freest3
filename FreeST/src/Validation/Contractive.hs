{-|
Module      :  Validation.Contractive
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Contractive
  ( contractive -- Contractive(..)
  )
where

import           Syntax.TypeVariable
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Validation.Terminated

-- class Contractive a where
--   contractive :: Set.Set TypeVar -> TypeVar -> a -> Bool

-- instance Contractive T.Type where

contractive :: K.PolyVars -> TypeVar -> T.Type -> Bool
contractive s a (T.Semi _ t u)
  | terminated s t                            = contractive s a u
  | otherwise                                 = contractive s a t
contractive s a (T.Rec _ (K.Bind _ _ _ t))    = contractive s a t
contractive s a (T.Forall _ (K.Bind _ _ _ t)) = contractive s a t
contractive _ a (T.Var _ b)                   = a /= b
contractive _ _ (T.Skip _)                    = False
contractive _ _ _                             = True

-- instance Contractive t => Contractive (K.Bind t) where
--   contractive a (K.Bind _ _ _ t) = contractive a t
