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
( Contractive(..)
)
where

import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Syntax.TypeVariable
import           Validation.Terminated

class Contractive a where
  contractive :: TypeVar -> a -> Bool

instance Contractive T.Type where
  contractive a (T.Semi _ t u)
    | terminated t = contractive a u
    | otherwise    = contractive a t
  contractive a (T.Rec _ b) = contractive a b
  contractive a (T.Forall _ b) = contractive a b
  contractive a (T.Var _ b) = a /= b
  contractive _ (T.Skip _) = False
  contractive _ _ = True

instance Contractive t => Contractive (K.Bind t) where
  contractive a (K.Bind _ _ _ t) = contractive a t
