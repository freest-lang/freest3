{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

module Equivalence.Normalisation
( Normalise(..)
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Normalise t where
  normalise :: TypeEnv -> t -> t

instance Normalise TypeScheme where
  normalise tenv (TypeScheme p bs t) = TypeScheme p bs (normalise tenv t)

instance Normalise Type where
    -- Functional types
  normalise tenv (Fun p q t u)    = Fun p q (normalise tenv t) (normalise tenv u)
  normalise tenv (PairType p t u) = PairType p (normalise tenv t) (normalise tenv u)
  normalise tenv t@(Datatype p m) = t -- We do not normalise under Datatype or we'll loop until eternity
    -- Session types
  normalise tenv (Semi _ (Choice p q m) t) =
    Choice p q (Map.map (\u -> append (normalise tenv u) t') m)
    where t' = normalise tenv t
  normalise tenv (Semi p t u) = append (normalise tenv t) (normalise tenv u)
  normalise tenv (Choice p q m) = Choice p q (Map.map (normalise tenv) m)
  normalise tenv (Rec p (TypeVarBind q x k) t)
    | x `Set.member` (free t) = normalise tenv $ unfold $ Rec p (TypeVarBind q x k) t'
    | otherwise               = t'
    where t' = normalise tenv t
    -- Functional or session
    -- Type operators
  normalise tenv (Dualof _ t) = normalise tenv (dual t)
  normalise tenv (TypeName _ a) = normalise tenv t
    where (_, TypeScheme _ [] t) = tenv Map.! a
    -- Otherwise: Basic, Skip, Message, TypeVar
  normalise tenv t = t

append :: Type -> Type -> Type
append (Skip _)       t = t
append t       (Skip _) = t
append (Semi p t u)   v = Semi p t (append u v)
append (Choice q v m) t = Choice q v (Map.map (`append` t) m)
append t              u = Semi (position t) t u

-- The set of free type variables in a type
free :: Type -> Set.Set TypeVar
  -- Functional types
free (Basic _ _)      = Set.empty
free (Fun _ _ t u)    = Set.union (free t) (free u)
free (PairType _ t u) = Set.union (free t) (free u)
free (Datatype _ m)   = Map.foldr (Set.union . free) Set.empty m
  -- Session types
free (Skip _)         = Set.empty
free (Semi _ t u)     = Set.union (free t) (free u)
free (Message _ _ _)  = Set.empty
free (Choice _ _ m)   = Map.foldr (Set.union . free) Set.empty m
free (Rec _ (TypeVarBind _ x _) t) = Set.delete x (free t)
  -- Functional or session
free (TypeVar _ x)    = Set.singleton x
  -- Type operators
free (Dualof _ t)     = free t
free (TypeName _ _)   = Set.empty
