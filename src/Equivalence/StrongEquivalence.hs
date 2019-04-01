{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

A notion of equivalence stronger than the bisimulation on session
types. This relation is strictly included in equivalence. Use it to
check whether two types are equivalent, prior to trying the
equivalence algorithm, which takes significantly more time.

-}

module Equivalence.StrongEquivalence
( strongEquiv
) where

import           Syntax.Bind
import           Syntax.Kinds
import           Syntax.Types
import           Syntax.Programs
import           Parse.Lexer (position)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

strongEquiv :: TypeEnv -> Type -> Type -> Bool
strongEquiv tenv t u = (normalise tenv t) == (normalise tenv u)

normalise :: TypeEnv -> Type -> Type
  -- Functional types
normalise tenv (Fun p q t u)    = Fun p q (normalise tenv t) (normalise tenv u)
normalise tenv (PairType p t u) = PairType p (normalise tenv t) (normalise tenv u)
normalise tenv (Datatype p m)   = Datatype p (Map.map (normalise tenv) m)
  -- Session types
normalise tenv (Semi _ (Choice p q m) t) =
  Choice p q (Map.map (\u -> append (normalise tenv u) t') m)
  where t' = normalise tenv t
normalise tenv (Semi p t u)     = append (normalise tenv t) (normalise tenv u)
normalise tenv (Choice p q m)   = Choice p q (Map.map (normalise tenv) m)
normalise tenv (Rec p (TBindK q x k) t)
  | x `Set.member` (free t)     = Rec p (TBindK q x k) t'
  | otherwise                   = t'
  where t' = normalise tenv t
  -- Functional or session
  -- Type operators
normalise tenv (Dualof _ t)     = normalise tenv (dual t)
normalise tenv (Name p c)       = normalise tenv t
  where (_, TypeScheme _ [] t) = tenv Map.! (TBind p c) -- TODO: polymorphic type names
  -- Otherwise: Basic, Skip, Message, Var
normalise tenv t                = t

append :: Type -> Type -> Type
append (Skip _)       t = t
append t       (Skip _) = t
append (Semi p t u)   v = Semi p t (append u v)
append (Choice q v m) t = Choice q v (Map.map (`append` t) m)
append t              u = Semi (position t) t u

-- The set of free type variables in a type
free :: Type -> Set.Set TVar
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
free (Rec _ (TBindK _ x _) t) = Set.delete x (free t)
  -- Functional or session
free (Var _ x)        = Set.singleton x
  -- Type operators
free (Dualof _ t)     = free t
free (Name _ _)       = Set.empty
