{-|
Module      :  Contractive
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

import           Syntax.Types
import           Syntax.TypeVariables
import           Syntax.Base (position)
import           Validation.Terminated (terminated)
import           Utils.FreestState
import           Control.Monad (unless)

-- Check the contractivity of a given type; issue an error if not
class Contractive t where
  checkContractive :: TypeVar -> t -> FreestState ()

instance Contractive Type where
  checkContractive a t =
    unless (contractive a t) $
     addError (position t) [Error "Type", Error t, Error "is not contractive on type variable", Error a]

-- A better notion of contractivy
contractive :: TypeVar -> Type -> Bool
contractive a (Semi _ t u)
    | terminated t = contractive a u
    | otherwise    = contractive a t
contractive a (Rec _ _ t) = contractive a t
contractive a (TypeVar _ b) = a /= b
contractive _ (Skip _) = False
contractive _ _ = True
