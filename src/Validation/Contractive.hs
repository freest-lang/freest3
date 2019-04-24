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
, contractive
)
where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Syntax.Show
import           Equivalence.Normalisation (terminated)
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
    when (not (contractive tEnv kEnv t)) $
     addError (position t) ["Type", styleRed $ show t, "is not contractive"]

instance Contractive TypeScheme where
  checkContractive kEnv (TypeScheme _ _ t) = checkContractive kEnv t

-- Is a given type contractive?

type Visited = Set.Set TypeVar

-- Revised version wrt to ICFP'16
contractive :: TypeEnv -> KindEnv -> Type -> Bool
contractive = contr Set.empty
  where
  contr :: Visited -> TypeEnv -> KindEnv -> Type -> Bool
  -- Session types
  contr _ _ _ (Skip _) = False
  contr v tEnv kEnv (Semi _ t u)
    | terminated t = contr v tEnv kEnv u
    | otherwise    = contr v tEnv kEnv t
  -- Functional or session
  contr v tEnv kEnv (Rec _ _ t)   = contr v tEnv kEnv t
  contr v tEnv kEnv (TypeVar p x) =
    Map.findWithDefault (kindSU p) x kEnv == kindSL p
  -- Type operators
  contr v tEnv kEnv (Dualof _ t) = contr v tEnv kEnv t
  contr v tEnv kEnv (TypeName p x)  -- TODO: not quite sure of this
    | x `Set.member` v    = False
    | x `Map.member` tEnv = contr (Set.insert x v) tEnv kEnv (getType (tEnv Map.! x))
    | otherwise           = True
  -- Functional types; Session Basic + Message + Choice
  contr _ _ _ _ = True

getType :: (Kind, TypeScheme) -> Type
getType (_, TypeScheme _ _ t) = t
