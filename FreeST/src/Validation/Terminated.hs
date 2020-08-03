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

module Validation.Terminated
( terminated
)
where

import           Syntax.Types

-- As in the ICFP'16 paper, except that no substitution is applied to Rec types
terminated :: Type -> Bool
terminated (Skip _)     = True
terminated (Semi _ t u) = terminated t && terminated u
terminated (Rec _ _ t)  = terminated t
terminated _            = False

{- A "better" terminated predicate should be such that only free variables *of kind SU* are terminated

terminated :: KindEnv -> Type ->  Bool
terminated kEnv t = term t
  where
  term (Skip _)      = True
  term (Semi _ s t)  = term s && term t
  term (Rec _ _ t)   = term t
  term (TypeVar _ x) = isUn (kEnv Map.! x)
  term _             = False
-}
