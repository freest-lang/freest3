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


-- [t/a]u, substitute t for for every occurrence of a in u

class Subs a where
  subs :: T.Type -> Variable -> a -> a
  -- apply all substitutions in σ to u; no renaming
  subsAll :: [(T.Type, Variable)] -> a -> a
  subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ

instance Subs  T.Type where
  -- Labelled
  subs t a (T.Labelled s k m) = T.Labelled s k (Map.map (subs t a) m)
  -- Functional types
  subs t a (T.Message s pol t1) = T.Message s pol (subs t a t1)
  subs t a (T.Arrow s m t1 t2) = T.Arrow s m (subs t a t1) (subs t a t2)
  -- Session types
  subs t a (T.Semi s t1 t2) = T.Semi s (subs t a t1) (subs t a t2)
  -- Polymorphism and recursion
  subs t a (T.Rec s b) = T.Rec s (subs t a b)
  subs t a (T.Forall s b) = T.Forall s (subs t a b)
  subs t a u@(T.Var _ y)
    | y == a    = t
    | otherwise = u
  subs (T.Var _ t) a u@(T.Dualof s1 (T.Var s2 b))
    | a == b    = T.Dualof s1 $ T.Var s2 t
    | otherwise = u
  subs t a u@(T.Dualof s1 (T.Var s2 b))
    | a == b    = dualof t
    | otherwise = u
  subs _ _ t = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Typing.Substitution.subs" t

instance (Subs t) => Subs (Bind k t) where
  subs t a (Bind p y k u) = Bind p y k (subs t a u)

-- [t/co-a]u, substitute t for for every occurrence of covariable a in u

class Cosubs a where
  cosubs :: T.Type -> Variable -> a -> a

instance Cosubs T.Type where
  -- Functional types
  cosubs t a (T.Message s pol t1) = T.Message s pol (cosubs t a t1)
  cosubs t a (T.Arrow s m t1 t2 ) = T.Arrow s m (cosubs t a t1) (cosubs t a t2)
  -- Session types
  cosubs t a (T.Semi s t1 t2) = T.Semi s (cosubs t a t1) (cosubs t a t2)
  cosubs t a (T.Labelled s k m) = T.Labelled s k (Map.map (cosubs t a) m)
    -- Polymorphism and recursion
  cosubs t a (T.Rec s b) = T.Rec s (cosubs t a b)
  cosubs t a (T.Forall s b) = T.Forall s (cosubs t a b)
  cosubs t a u@(T.Dualof _ (T.Var _ b))
    | a == b    = t
    | otherwise = u
  cosubs _ _ t = t

instance Cosubs t => Cosubs (Bind K.Kind t) where
  cosubs t a (Bind p y k u) = Bind p y k (cosubs t a u)


-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold t@(T.Rec _ (Bind _ a _ u)) = subs t a u
unfold t = internalError "Typing.Substitution.unfold" t

-- The set of free type variables in a type
free :: T.Type -> Set.Set Variable
free (T.Arrow _ _ t u) = free t `Set.union` free u
free (T.Labelled _ _ m) =
  Map.foldr (\t acc -> free t `Set.union` acc) Set.empty m
free (T.Message _ _ t) = free t 
free (T.Semi _ t u) = free t `Set.union` free u
free (T.Rec _ (Bind _ a _ t)) = Set.delete a (free t)
free (T.Forall _ (Bind _ a _ t)) = Set.delete a (free t)
free (T.Var _ x) = Set.singleton x
free (T.Dualof _ x) = free x
free _ = Set.empty 
