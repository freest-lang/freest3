module Validation.Contractive (checkContractive) where

import qualified Data.Map.Strict as Map
import           Syntax.Types
import           Utils.Errors
import           Validation.TypingState

-- Is a given type contractive?
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi _ t _) = contractive kenv t
contractive kenv (Rec _ _ t)  = contractive kenv t
contractive kenv (Var _ x)    = Map.member x kenv
contractive _    _            = True


-- Check the contractivity of a given type; issue an error if not
checkContractive :: KindEnv -> Type -> TypingState ()
checkContractive kenv t
  | contractive kenv t = return ()
  | otherwise          = 
      addError (typePos t) ["Type", styleRed $ show t, "is not contractive"]
