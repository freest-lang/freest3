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

module Validation.Contractive
( Contractive(..)
)
where

-- import           Syntax.Schemes
import           Syntax.Types
-- import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base (position)
-- import           Syntax.Show
import           Validation.Terminated (terminated)
-- import           Utils.ErrorMessage
import           Utils.FreestState
import           Control.Monad (when)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set

-- Check the contractivity of a given type; issue an error if not
class Contractive t where
  checkContractive :: TypeVar -> t -> FreestState ()

instance Contractive Type where
  checkContractive x t =
    when (not (contractive x t)) $
     addError (position t) [Error "Type", Error t, Error "is not contractive"]

-- A better notion of contractivy
contractive :: TypeVar -> Type -> Bool
-- Session types
contractive _ (Skip _) = False
contractive x (Semi _ t u)
    | terminated t = contractive x u
    | otherwise    = contractive x t
-- Recursive types
contractive x (TypeVar _ y) = x /= y
contractive x (Rec _ _ t) = contractive x t
-- Functional and session types
contractive _    _              = True

{-
class Contractive t where
  checkContractive :: KindEnv -> t -> FreestState ()

-- Check the contractivity of a given type; issue an error if not
instance Contractive Type where
  checkContractive kEnv t = do
    tEnv <- getTEnv
    when (not (contractive tEnv kEnv t)) $
     addError (position t) [Error "Type", Error t, Error "is not contractive"]
--     addError (position t) ["Type", styleRed $ show t, "is not contractive"]

instance Contractive TypeScheme where
  checkContractive kEnv (TypeScheme _ xks t) = checkContractive (insert kEnv xks) t

-- Is a given type contractive?

-- Revised version wrt to ICFP'16

type Visited = Set.Set TypeVar

contractive :: TypeEnv -> KindEnv -> Type -> Bool
contractive tEnv kEnv = contr Set.empty
  where
  contr :: Visited -> Type -> Bool
  -- Session types
  contr _ (Skip _) = False
  contr v (Semi _ t u)
    | terminated t = contr v u
    | otherwise    = contr v t
  -- Functional or session
  contr v (Rec _ _ t) = contr v t
  contr v (TypeVar p x) = Map.findWithDefault (kindSU p) x kEnv == kindSL p
  -- Type operators
  contr v (Dualof _ t) = contr v t
  contr v (TypeName p x)  -- TODO: not quite sure of this
    | x `Set.member` v    = False
    | x `Map.member` tEnv = contr (Set.insert x v) (getType (tEnv Map.! x))
    | otherwise           = True
  -- Otherwise: functional types, Message, Choice
  contr _ _ = True

getType :: (Kind, TypeScheme) -> Type
getType (_, TypeScheme _ _ t) = t
-}
