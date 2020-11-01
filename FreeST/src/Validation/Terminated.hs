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

module Validation.Terminated
( terminated
)
where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import qualified Data.Set as Set

-- A terminated type is composed of Skip, semi-colon, recursive types,
-- and variables introduced by recursive types. In particular infinite
-- sequences of Skips is terminated.
terminated :: Type -> Bool
terminated = term Set.empty
  where
    term _ (Skip _) = True
    term s (Semi _ t u) = term s t && term s u
    term s (Rec _ (KindBind _ a _) t) = contractive a t && term (Set.insert a s) t
    term s (TypeVar _ a) = a `Set.member` s
    term _ _ = False

-- TODO: duplicated code; see Validation.Contractive. These two modules must be joined together
contractive :: TypeVar -> Type -> Bool
contractive a (Semi _ t u)
  | terminated t = contractive a u
  | otherwise    = contractive a t
contractive a (Rec _ _ t) = contractive a t
contractive a (TypeVar _ b) = a /= b
contractive _ (Skip _) = False
contractive _ _ = True
