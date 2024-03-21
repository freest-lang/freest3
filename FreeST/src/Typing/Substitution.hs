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
  )
where


import           Syntax.Base
import qualified Syntax.Kind         as K
import qualified Syntax.Type         as T
import           Elaboration.Duality ( dualof )
import           Util.Error          ( internalError )

import qualified Data.Map.Strict as Map


-- [t/a]u, substitute t for for every occurrence of a in u

class Subs a where
  subs :: T.Type -> Variable -> a -> a
  subsAll :: [(T.Type, Variable)] -> a -> a
  subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ

instance Subs T.Type where
  -- Functional types
  subs t a (T.Arrow s m t1 t2) = T.Arrow s m (subs t a t1) (subs t a t2)
  subs t a (T.Labelled s k m) = T.Labelled s k (Map.map (subs t a) m)
  -- Session types
  subs t a (T.Semi s t1 t2) = T.Semi s (subs t a t1) (subs t a t2)
  subs t a (T.Message s pol t1) = T.Message s pol (subs t a t1)
  -- Polymorphism and recursion
  subs t a (T.Rec s b) = T.Rec s (subs t a b)
  subs t a (T.Forall s b) = T.Forall s (subs t a b)
  subs t a u@(T.Var _ y)
    | y == a    = t
    | otherwise = u
  subs (T.Var _ t) a u@(T.Dualof s1 (T.Var s2 b))
    | a == b    = T.Dualof s1 $ T.Var s2 t
    | otherwise = u
  -- Type operators
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
  cosubs t a (T.Arrow s m t1 t2 ) = T.Arrow s m (cosubs t a t1) (cosubs t a t2)
  cosubs t a (T.Labelled s k m) = T.Labelled s k (Map.map (cosubs t a) m)
  -- Session types
  cosubs t a (T.Semi s t1 t2) = T.Semi s (cosubs t a t1) (cosubs t a t2)
  cosubs t a (T.Message s pol t1) = T.Message s pol (cosubs t a t1)
    -- Polymorphism and recursion
  cosubs t a (T.Rec s b) = T.Rec s (cosubs t a b)
  cosubs t a (T.Forall s b) = T.Forall s (cosubs t a b)
  -- Type operators
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
