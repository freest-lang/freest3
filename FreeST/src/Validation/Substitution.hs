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
  -- , free
  )
where

import           Syntax.TypeVariable
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Utils.Error                    ( internalError )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

-- [t/x]u, substitute t for for every occurrence of x in u
-- Assume types were renamed (hence, x/=y and no -the-fly renaming needed)
subs :: T.Type -> TypeVar -> T.Type -> T.Type
-- Functional types
subs t x (T.Fun p m t1 t2) = T.Fun p m (subs t x t1) (subs t x t2)
subs t x (T.Pair p t1 t2) = T.Pair p (subs t x t1) (subs t x t2)
subs t x (T.Datatype p m) = T.Datatype p (Map.map (subs t x) m)
-- Session types
subs t x (T.Semi p t1 t2) = T.Semi p (subs t x t1) (subs t x t2)
subs t x (T.Choice p v  m) = T.Choice p v (Map.map (subs t x) m)
  -- Polymorphism and recursion
subs t x (T.Rec p yk u) = T.Rec p yk (subs t x u) 
subs t x (T.Forall p yk u) = T.Forall p yk (subs t x u)
subs t x u@(T.Var _ y)
  | y == x = t
  | otherwise = u
subs _ _ t@T.Dualof{} = internalError "Validation.Substitution.subs" t
subs _ _ t = t

-- subsAll σ u, apply all substitutions in σ to u; no renaming
subsAll :: [(T.Type, TypeVar)] -> T.Type -> T.Type
subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ

-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold t@(T.Rec _ (K.Bind _ x _) u) = subs t x u
unfold t = internalError "Validation.Substitution.unfold" t

{-

-- Not needed. Cf. Validation.Renam.isFreeIn.
-- The set of free type variables in a type
free :: T.Type -> Set.Set TypeVar
  -- Functional types
free (T.Fun _ _ t u) = free t `Set.union` free u
free (T.Pair _ t u) = free t `Set.union` free u
free (T.Datatype _ m) = freeMap m
  -- Session types
free (T.Semi _ t u) = free t `Set.union` free u
free (T.Choice _ _ m) = freeMap m
  -- Functional or session
free (T.Rec _ (K.Bind _ x _) t) = Set.delete x (free t)
free (T.Var _ x) = Set.singleton x
  -- T.Type operators
free t@T.Dualof{} = internalError "Validation.Substitution.free" t
  -- Otherwise: Basic, Skip, Message
free _ = Set.empty

freeMap :: T.TypeMap -> Set.Set TypeVar
freeMap = Map.foldr (\t acc -> free t `Set.union` acc) Set.empty

Define [t/x]u to be the result of substituting t for every free
occurrence of x in u, and changing bound variables to avoid clashes
[Hindley&Seldin 1986, Definition 1.11]

Does not work with bisimilarity, for substitution does not preserve
the is-renamed predicate.

subs :: T.Type -> T.TypeVar -> T.Type -> T.Type
  -- Functional types
subs t x (Fun p m u v)    = Fun p m (subs t x u) (subs t x v)
subs t x (Pair p u v) = Pair p (subs t x u) (subs t x v)
subs t x (Datatype p m)   = Datatype p (Map.map (subs t x) m)
  -- Session types
subs t x (Semi p u v)     = Semi p (subs t x u) (subs t x v)
subs t x (Choice p v m)   = Choice p v (Map.map (subs t x) m)
subs t x u@(Rec p yk@(K.Bind q y k) v)
  | y == x                = u
  -- | y `Set.notMember` (free t) || x `Set.notMember` (free v) = Rec p yk (subs t x v)
  | otherwise             = Rec p (K.Bind q z k) (subs t x (subs (TypeVar q z) y v))
    where z = mkNewVar 0 y
  -- Functional or session
subs t x u@(TypeVar _ y)
  | y == x                = t
  | otherwise             = u
  -- T.Type operators  
subs t x (Dualof p u)     = Dualof p (subs t x u)
  -- Otherwise: Basic, Skip, Message, T.TypeName
subs _ _ t                = t

-}
