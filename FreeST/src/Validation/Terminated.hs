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

import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import qualified Data.Set                      as Set

terminated :: K.PolyVars -> T.Type -> Bool
terminated _ (T.Skip _                 ) = True
terminated s (T.Semi _ t u             ) = terminated s t && terminated s u
terminated s (T.Rec  _ (K.Bind _ _ _ t)) = terminated s t
terminated _ _                           = False

