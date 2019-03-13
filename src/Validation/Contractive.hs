{-|
Module      :  Kinding
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Contractive
( checkContractive
)
where

import           Syntax.Programs
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Utils.Errors
import           Validation.TypingState
import qualified Data.Map.Strict as Map

-- Is a given type contractive?
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi _ t _) = contractive kenv t
contractive kenv (Rec _ _ t)  = contractive kenv t
contractive kenv (Var p x)    = Map.member (Bind p x) kenv
contractive _    _            = True

-- Check the contractivity of a given type; issue an error if not
checkContractive :: Type -> TypingState ()
checkContractive t = do
  kenv <- getKenv
  if contractive kenv t
  then return ()
  else addError (position t) ["Type", styleRed $ show t, "is not contractive"]
