{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{- |
Module      :  Typing.Substitution
Description :  Type substitution
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

We assume types were renamed (hence, x/=y and no -the-fly renaming needed).

-}

module Typing.Substitution
  ( subs
  , cosubs
  , subsAll
  , unfold
  , free
  )
where

-- import           Elaboration.Duality
import qualified Data.Map.Strict as Map
import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error ( internalError )
import           Elaboration.Duality
import qualified Data.Set as Set


-- [t/x]u, substitute t for for every occurrence of x in u

class Subs a where
  subs :: T.Type -> Variable -> a -> a
  -- apply all substitutions in σ to u; no renaming
  subsAll :: [(T.Type, Variable)] -> a -> a
  subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ

instance Subs  T.Type where
  -- Labelled
  subs t x (T.Labelled s k m) = T.Labelled s k (Map.map (subs t x) m)
  -- Functional types
  subs t x (T.Message s pol t1) = T.Message s pol (subs t x t1)
  subs t x (T.Arrow s m t1 t2) = T.Arrow s m (subs t x t1) (subs t x t2)
  -- Session types
  subs t x (T.Semi s t1 t2) = T.Semi s (subs t x t1) (subs t x t2)
  -- Polymorphism and recursion
  subs t x (T.Rec s b) = T.Rec s (subs t x b)
  subs t x (T.Forall s b) = T.Forall s (subs t x b)
  subs t x u@(T.Var _ y)
    | y == x    = t
    | otherwise = u
  subs (T.Var _ t) x u@(T.Dualof s (T.Var p' y))
    | y == x    = T.Dualof s $ T.Var p' t
    | otherwise = u
  subs t x u@(T.Dualof s (T.Var p' y))
    | y == x    = dualof t
    | otherwise = u
  subs _ _ t = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Typing.Substitution.subs" t

instance (Subs t) => Subs (Bind k t) where
  subs t x (Bind p y k u) = Bind p y k (subs t x u)

-- CoVar subs, [t/co-x]u

class Cosubs t where
  cosubs :: T.Type -> Variable -> t -> t

instance Cosubs T.Type where
  -- Functional types
  cosubs t x (T.Message s pol t1) = T.Message s pol (cosubs t x t1)
  cosubs t x (T.Arrow s m t1 t2 ) = T.Arrow s m (cosubs t x t1) (cosubs t x t2)
  -- Session types
  cosubs t x (T.Semi s t1 t2) = T.Semi s (cosubs t x t1) (cosubs t x t2)
  cosubs t x (T.Labelled s k m) = T.Labelled s k (Map.map (cosubs t x) m)
    -- Polymorphism and recursion
  cosubs t x (T.Rec s b) = T.Rec s (cosubs t x b)
  cosubs t x (T.Forall s b) = T.Forall s (cosubs t x b)
  cosubs t x u@(T.Dualof _ (T.Var _ y))
    | y == x = t
    | otherwise = u
  cosubs _ _ t = t

instance Cosubs t => Cosubs (Bind K.Kind t) where
  cosubs t x (Bind p y k u) = Bind p y k (cosubs t x u)


-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold t@(T.Rec _ (Bind _ x _ u)) = subs t x u
unfold t = internalError "Typing.Substitution.unfold" t

-- The set of free type variables in a type
free :: T.Type -> Set.Set Variable
free (T.Arrow _ _ t u) = free t `Set.union` free u
free (T.Labelled _ _ m) =
  Map.foldr (\t acc -> free t `Set.union` acc) Set.empty m
free (T.Message _ _ t) = free t 
free (T.Semi _ t u) = free t `Set.union` free u
free (T.Rec _ (Bind _ x _ t)) = Set.delete x (free t)
free (T.Forall _ (Bind _ x _ t)) = Set.delete x (free t)
free (T.Var _ x) = Set.singleton x
free (T.Dualof _ x) = free x
free _ = Set.empty 
