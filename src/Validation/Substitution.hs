{- |
Module      :  Position
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Unfolding of recursive types and substitution
-}

module Validation.Substitution
( subs
, subsAll
, unfold
, free
) where

import           Syntax.Base
import           Syntax.TypeVariables
import           Syntax.Kinds
import           Syntax.Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import           Debug.Trace
import           Syntax.Show

-- [t/x]u, substitute t for for every free occurrence of x in u
-- Notice that this operation does not preserve renaming
subs :: Type -> TypeVar -> Type -> Type
-- Functional types
subs t x (Fun p m t1 t2)    = Fun p m (subs t x t1) (subs t x t2)
subs t x (PairType p t1 t2) = PairType p (subs t x t1) (subs t x t2)
subs t x (Datatype p m)     = Datatype p (Map.map(subs t x) m)
-- Session types
subs t x (Semi p t1 t2)     = Semi p (subs t x t1) (subs t x t2)
subs t x (Choice p v m)     = Choice p v (Map.map(subs t x) m)
subs t x (Rec p yk u)       = trace (show yk) $ Rec p yk (subs t x u) -- Assume types were renamed (hence, x/=y and no -the-fly renaming needed)
-- Functional or session
subs t x u@(TypeVar _ y)
  | y == x                 = t
  | otherwise              = u
subs t x (Dualof p u)      = Dualof p (subs t x u)
subs _ _ t                 = t

-- subsAll σ u, apply all substitutions in σ to u; no renaming
-- Notice that this operation does not preserve renaming
subsAll :: [(Type, TypeVar)] -> Type -> Type
subsAll σ s = List.foldl' (\u (t, x) -> subs t x u) s σ

-- Unfold a recursive type (one step only)
-- Notice that this operation does not preserve renaming
unfold :: Type -> Type
unfold t@(Rec _ (TypeVarBind _ x _) u) = subs t x u

-- The set of free type variables in a type
free :: Type -> Set.Set TypeVar
  -- Functional types
free (Fun _ _ t u) = Set.union (free t) (free u)
free (PairType _ t u) = Set.union (free t) (free u)
free (Datatype _ m) = freeMap m
  -- Session types
free (Semi _ t u) = Set.union (free t) (free u)
free (Choice _ _ m) = freeMap m
  -- Functional or session
free (Rec _ (TypeVarBind _ x _) t) = Set.delete x (free t)
free (TypeVar _ x) = Set.singleton x
  -- Type operators
free (TypeName _ _) = Set.empty -- TODO: fix me!
free (Dualof _ t) = free t
  -- Otherwise: Basic, Skip, Message
free _ = Set.empty

freeMap :: TypeMap -> Set.Set TypeVar
freeMap = Map.foldr (\t acc -> (free t) `Set.union` acc) Set.empty

{-

Define [t/x]u to be the result of substituting t for every free
occurrence of x in u, and changing bound variables to avoid clashes
[Hindley&Seldin 1986, Definition 1.11]

Does not work with bisimilarity, for substitution does not preserve
the is-renamed predicate.

subs :: Type -> TypeVar -> Type -> Type
  -- Functional types
subs t x (Fun p m u v)    = Fun p m (subs t x u) (subs t x v)
subs t x (PairType p u v) = PairType p (subs t x u) (subs t x v)
subs t x (Datatype p m)   = Datatype p (Map.map (subs t x) m)
  -- Session types
subs t x (Semi p u v)     = Semi p (subs t x u) (subs t x v)
subs t x (Choice p v m)   = Choice p v (Map.map (subs t x) m)
subs t x u@(Rec p yk@(TypeVarBind q y k) v)
  | y == x                = u
  -- | y `Set.notMember` (free t) || x `Set.notMember` (free v) = Rec p yk (subs t x v)
  | otherwise             = Rec p (TypeVarBind q z k) (subs t x (subs (TypeVar q z) y v))
    where z = mkNewVar 0 y
  -- Functional or session
subs t x u@(TypeVar _ y)
  | y == x                = t
  | otherwise             = u
  -- Type operators  
subs t x (Dualof p u)     = Dualof p (subs t x u)
  -- Otherwise: Basic, Skip, Message, TypeName
subs _ _ t                = t

-}
