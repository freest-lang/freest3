{-|
Module      :  Validation.Terminated
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

import           Syntax.Types
-- import           Syntax.Kinds
import           Syntax.TypeVariables
import           Validation.Terminated

contractive :: TypeVar -> Type -> Bool
contractive a (Semi _ t u)
  | terminated t = contractive a u
  | otherwise    = contractive a t
contractive a (Rec _ _ t) = contractive a t
contractive a (TypeVar _ b) = a /= b
contractive _ (Skip _) = False
contractive _ _ = True
