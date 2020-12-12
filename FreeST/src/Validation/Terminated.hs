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

import qualified Data.Set                      as Set
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Validation.Subkind            ( isSession )

terminated :: T.Type -> Bool
terminated = term Set.empty
 where
  term _ (T.Skip _                 ) = True
  term s (T.Semi _ t              u) = term s t && term s u
  term s (T.Rec  _ (K.Bind _ a k) t) = isSession k && term (Set.insert a s) t
  term s (T.TypeVar _ a            ) = a `Set.member` s
  term _ _                           = False

