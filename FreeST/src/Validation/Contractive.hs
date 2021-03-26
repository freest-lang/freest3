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

import           Syntax.TypeVariable
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Validation.Terminated
import qualified Data.Set as Set

contractive :: K.PolyVars -> TypeVar -> T.Type -> Bool
contractive s a (T.Semi _ t u)
  | terminated s t                            = contractive s a u
  | otherwise                                 = contractive s a t
contractive s a (T.Rec _ (K.Bind _ _ _ t))    = contractive s a t
contractive s a (T.Forall _ (K.Bind _ _ _ t)) = contractive s a t
contractive s a (T.Var _ b)                   = b `Set.notMember` s && a /= b
contractive _ _ _                             = True

