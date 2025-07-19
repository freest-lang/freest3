{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
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
  , subsLevelInType
  )
where


import           Syntax.Base
import qualified Syntax.Kind         as K
import qualified Syntax.Type         as T
import           Elaboration.Duality ( dualof )
import           Util.Error          ( internalError )

import qualified Data.Map.Strict as Map
import           Debug.Trace (trace)


-- [t/a]u, substitute t for for every occurrence of a in u

class Subs a where
  subs :: T.Type -> Variable -> a -> a
  subsLevel :: T.Level -> Variable -> a -> a
  subsAll :: [(T.Type, Variable)] -> a -> a
  subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ

instance Subs T.Type where
  -- Functional types
  subs t a (T.Arrow s m l1 l2 t1 t2) = T.Arrow s m l1 l2 (subs t a t1) (subs t a t2)
  subs t a (T.Labelled s k l m) = T.Labelled s k l (Map.map (subs t a) m)
  -- Session types
  subs t a (T.Semi s t1 t2) = T.Semi s (subs t a t1) (subs t a t2)
  subs t a (T.Message s l pol t1) = T.Message s l pol (subs t a t1)
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
  subs t a u@(T.Dualof _ (T.Var _ b))
    | a == b    = dualof t
    | otherwise = u
  -- Priority Polymorphism
  subs t a (T.PForall s b) = T.PForall s (subs t a b)
  subs _ _ t = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Typing.Substitution.subs" t

instance (Subs t) => Subs (Bind k t) where
  subs t a (Bind p b k u) = Bind p b k (subs t a u)

instance Subs T.Level where
  -- Level variables
  subsLevel l a (T.LVar x)
    -- | (extern x) == (extern a) = trace (extern x ++ " " ++ show l) l
    | (extern x) == (extern a) = l
    | otherwise = T.LVar x
  -- Level addition and parentheses
  subsLevel l a (T.LAdd l1 l2) = T.LAdd (subsLevel l a l1) (subsLevel l a l2)
  subsLevel l a (T.LParens l1) = T.LParens (subsLevel l a l1)
  -- Other levels remain unchanged
  subsLevel _ _ l = l

subsLevelInType :: T.Level -> Variable -> T.Type -> T.Type
-- Functional types
subsLevelInType l a t@(T.Arrow s m l1 l2 t1 t2) = T.Arrow s m (subsLevel l a l1) (subsLevel l a l2) (subsLevelInType l a t1) (subsLevelInType l a t2)
subsLevelInType l a (T.Labelled s k l' m) = T.Labelled s k (subsLevel l a l') (Map.map (subsLevelInType l a) m)
-- Session types
subsLevelInType l a (T.End s p l') = T.End s p (subsLevel l a l')
subsLevelInType l a (T.Semi s t1 t2) = T.Semi s (subsLevelInType l a t1) (subsLevelInType l a t2)
subsLevelInType l a (T.Message s l' p t1) = T.Message s (subsLevel l a l') p (subsLevelInType l a t1)
-- Polymorphism and recursion
subsLevelInType l a (T.Forall s b) = T.Forall s (subsLevelInBind l a b)
subsLevelInType l a (T.Rec s b) = T.Rec s (subsLevelInBind l a b)
subsLevelInType l a u@(T.Var s b) = u 
-- Type operators
subsLevelInType l a u@(T.Dualof s t) = T.Dualof s (subsLevelInType l a t)
-- Priority Polymorphism
subsLevelInType l a (T.PForall s b) = T.PForall s (subsLevelInBindWithRange l a b)
subsLevelInType _ _ t = t

subsLevelInBindWithRange :: T.Level -> Variable -> Bind T.LevelRange T.Type -> Bind (T.Level, T.Level) T.Type
subsLevelInBindWithRange l a (Bind p b (l1, l2) u) = Bind p b (subsLevel l a l1, subsLevel l a l2) (subsLevelInType l a u)

subsLevelInBind :: T.Level -> Variable -> Bind k T.Type -> Bind k T.Type
subsLevelInBind l a (Bind p b k u) = Bind p b k (subsLevelInType l a u)

-- [t/co-a]u, substitute t for for every occurrence of covariable a in u

class Cosubs a where
  cosubs :: T.Type -> Variable -> a -> a

instance Cosubs T.Type where
  -- Functional types
  cosubs t a (T.Arrow s m l1 l2 t1 t2 ) = T.Arrow s m l1 l2 (cosubs t a t1) (cosubs t a t2)
  cosubs t a (T.Labelled s k l m) = T.Labelled s k l (Map.map (cosubs t a) m)
  -- Session types
  cosubs t a (T.Semi s t1 t2) = T.Semi s (cosubs t a t1) (cosubs t a t2)
  cosubs t a (T.Message s l pol t1) = T.Message s l pol (cosubs t a t1)
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
