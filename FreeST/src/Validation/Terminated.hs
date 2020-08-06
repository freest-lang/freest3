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

-- As in the ICFP'16 paper, except that Rec types are not terminated. Any type of the form 'rec a.T' with T terminated is not contractive, hence not well formed.
terminated :: Type -> Bool
terminated (Skip _)     = True
terminated (Semi _ t u) = terminated t && terminated u
-- terminated (Rec _ _ t)  = terminated t
terminated _            = False
