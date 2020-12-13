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
( contractive
)
where

import qualified Syntax.Type as T
import           Syntax.TypeVariable
import           Validation.Terminated

contractive :: TypeVar -> T.Type -> Bool
contractive a (T.Semi _ t u)
  | terminated t = contractive a u
  | otherwise    = contractive a t
contractive a (T.Rec _ _ t) = contractive a t
contractive a (T.Forall _ _ t) = contractive a t
contractive a (T.Var _ b) = a /= b
contractive _ (T.Skip _) = False
contractive _ _ = True
