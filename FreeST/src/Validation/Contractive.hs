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
  checkContractive x t =
    unless (contractive x t) $
     addError (position t) [Error "Type", Error t, Error "is not contractive on type variable", Error x]

-- A better notion of contractivy
contractive :: TypeVar -> Type -> Bool
-- Session types
contractive _ (Skip _) = False
contractive x (Semi _ t u)
    | terminated t = contractive x u
    | otherwise    = contractive x t
-- Recursive types
contractive x (TypeVar _ y) = x /= y
contractive x (Rec _ _ t) = contractive x t
-- Functional and session types
contractive _ _ = True
