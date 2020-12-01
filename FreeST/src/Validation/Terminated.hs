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
import qualified Syntax.Kind as K
import           Syntax.TypeVariables
import qualified Data.Set as Set

terminated = term Set.empty
  where
    term _ (Skip _) = True
    term s (Semi _ t u) = term s t && term s u
    term s (Rec _ (K.KindBind _ a _) t) = term (Set.insert a s) t
    term s (TypeVar _ a) = a `Set.member` s
    term _ _ = False
