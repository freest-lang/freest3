{- |
Module      :  Type Schemes
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Schemes
( TypeScheme(..)
, TypeEnv
, VarEnv
, toTypeScheme
, isSessionType
) where

import           Syntax.Types
import           Syntax.Kinds (Kind, TypeVarBind, KindEnv)
import           Syntax.ProgramVariables (ProgVar)
import           Syntax.TypeVariables (TypeVar)
import           Syntax.Base
import qualified Data.Map.Strict as Map
import           Data.List (intersperse)

data TypeScheme = TypeScheme Pos [TypeVarBind] Type

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (Kind, TypeScheme)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar TypeScheme

toTypeScheme :: Type -> TypeScheme
toTypeScheme t = TypeScheme (position t) [] t

-- instance Show TypeScheme where
--   show (TypeScheme _ [] t) = show t
--   show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
--     where bindings = concat $ intersperse ", " (map show bs)

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

instance Default TypeScheme where
 omission p = TypeScheme p [] (omission p)

-- Assumes the type is well formed
isSessionType :: TypeEnv -> KindEnv -> Type -> Bool
  -- Session types
isSessionType _    _    (Skip _)        = True
isSessionType _    _    (Semi _ _ _)    = True
isSessionType _    _    (Message _ _ _) = True
isSessionType _    _    (Choice _ _ _)  = True
isSessionType tenv kenv (Rec _ _ t)     = isSessionType tenv kenv t
  -- Functional or session
isSessionType _    kenv (TypeVar _ x)   = Map.member x kenv
  -- Type operators
isSessionType _    _    (Dualof _ _)    = True
isSessionType tenv kenv (TypeName p x)  = False -- isSession $ fst $ tenv Map.! x
  -- Otherwise: Functional types
isSessionType _    _    _               = False

