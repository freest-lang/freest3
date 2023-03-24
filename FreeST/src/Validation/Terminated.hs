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

import Syntax.Base                             (Bind(..))         
import qualified Syntax.Type                   as T

terminated :: T.Type -> Bool
terminated (T.Skip _              ) = True
terminated (T.Semi _ t u          ) = terminated t && terminated u
terminated (T.Rec _ (Bind _ _ _ t)) = terminated t
terminated _                        = False

