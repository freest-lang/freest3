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
( Contractive(..)
)
where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Syntax.Show
import           Utils.Errors
import           Utils.FreestState
import           Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Contractive t where
  checkContractive :: KindEnv -> t -> FreestState ()

-- Check the contractivity of a given type; issue an error if not
instance Contractive Type where
  checkContractive kEnv t = do
    tEnv <- getTEnv
    when (not (contractive Set.empty tEnv kEnv t)) $
     addError (position t) ["Type", styleRed $ show t, "is not contractive"]

instance Contractive TypeScheme where
  checkContractive kEnv (TypeScheme _ _ t) = checkContractive kEnv t

-- Is a given type contractive?

type Visited = Set.Set TypeVar

contractive :: Visited -> TypeEnv -> KindEnv -> Type -> Bool
contractive v tEnv kEnv (Semi _ t _)  = contractive v tEnv kEnv t
contractive v tEnv kEnv (Rec _ _ t)   = contractive v tEnv kEnv t
contractive v tEnv kEnv (Dualof _ t)  = contractive v tEnv kEnv t
contractive v tEnv kEnv (TypeVar _ x) = Map.member x kEnv
contractive v tEnv kEnv (TypeName p x)
  | x `Set.member` v                  = False
  | x `Map.member` tEnv               = contractive (Set.insert x v) tEnv kEnv (getType (tEnv Map.! x))
  | otherwise                         = True
contractive _ _    _    _             = True -- Freest will issue an error later (I hope)

getType :: (Kind, TypeScheme) -> Type
getType (_, TypeScheme _ _ t) = t
