{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

A notion of equivalence stronger than the bisimulation on context-free
session types. This relation is strictly included in equivalence. Use
it to check whether two types are equivalent, prior to trying the
equivalence algorithm, which takes significantly longer.

-}

module Equivalence.StrongEquivalence
( StrongEquiv(..)
, instantiate
) where

import           Syntax.Bind
import           Syntax.Kinds
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Programs
import           Parse.Lexer (position)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- The class, good for types and for type schemes (at least)

class StrongEquiv t where
  strongEquiv :: TypeEnv -> t -> t -> Bool

-- Strong equivalence for types

instance StrongEquiv Type where
  strongEquiv tenv t u = t == u -- normalise tenv t == normalise tenv u

normalise :: TypeEnv -> Type -> Type
  -- Functional types
normalise tenv (Fun p q t u)    = Fun p q (normalise tenv t) (normalise tenv u)
normalise tenv (PairType p t u) = PairType p (normalise tenv t) (normalise tenv u)
normalise tenv t@(Datatype p m) = t -- We do not normalise under Datatype or we'll loop until eternity
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
  -- Otherwise: Basic, Skip, Message, TypeVar
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
free (TypeVar _ x)    = Set.singleton x
  -- Type operators
free (Dualof _ t)     = free t
free (Name _ _)       = Set.empty

-- Strong equivalence for type schemes

instance StrongEquiv TypeScheme where
  strongEquiv tenv ts1 ts2 =
    case instantiate ts1 ts2 of
      Nothing          -> False
      Just (_, t1, t2) -> strongEquiv tenv t1 t2

instantiate :: TypeScheme -> TypeScheme -> Maybe (KindEnv, Type, Type)
instantiate (TypeScheme _ bs1 t1) (TypeScheme _ bs2 t2) = inst bs1 bs2 t1 t2
  where
  inst :: [TBindK] -> [TBindK] -> Type -> Type -> Maybe (KindEnv, Type, Type)
  inst ((TBindK p1 x1 k1):bs1) (tk2@(TBindK _ x2 k2):bs2) t1 t2
    | k1 /= k2  = Nothing
    | x1 == x2 = inst bs1 bs2 t1 (subs (TypeVar p1 x1) tk2 t2)
    | otherwise = -- substitute x1 for x2
        case inst bs1 bs2 t1 (subs (TypeVar p1 x1) tk2 t2) of
          Nothing -> Nothing
          Just (m, t1, t2) -> Just (Map.insert (TBind p1 x1) k1 m, t1, t2)
  inst [] [] t1 t2 = Just (Map.empty, t1, t2)
  inst _ _ _ _ = Nothing
