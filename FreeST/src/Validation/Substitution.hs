{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{- |
Module      :  Validation.Substitution
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Substitution and unfolding recursive types.

-}

module Validation.Substitution
  ( subs
  , cosubs
  , subsAll
  , unfold
  )
where

-- import           Elaboration.Duality
import qualified Data.Map.Strict as Map
import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error ( internalError )
import           Elaboration.Duality


-- [t/x]u, substitute t for for every occurrence of x in u
-- Assume types were renamed (hence, x/=y and no -the-fly renaming needed)

class Subs t x u where
  subs :: t -> x -> u -> u
  subsAll :: [(t, x)] -> u -> u
  subsAll σ s = foldl (\u (t, x) -> subs t x u) s σ  -- apply all substitutions in σ to u; no renaming

instance Subs T.Type Variable T.Type where
  -- Almanac
  subs t x (T.Almanac p s m   ) = T.Almanac p s (Map.map (subs t x) m)
  -- Functional types
  subs t x (T.Message p pol t1) = T.Message p pol (subs t x t1)
  subs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (subs t x t1) (subs t x t2)
  -- Session types
  subs t x (T.Semi   p t1 t2  ) = T.Semi p (subs t x t1) (subs t x t2)
  -- Polymorphism and recursion
  subs t x (T.Rec    p b      ) = T.Rec p (subs t x b)
  subs t x (T.Forall p b      ) = T.Forall p (subs t x b)
  subs t x u@(T.Var _ y)
    | y == x    = t
    | otherwise = u
  subs (T.Var _ t) x u@(T.Dualof p (T.Var p' y))
    | y == x    = T.Dualof p $ T.Var p' t
    | otherwise = u
  subs t x u@(T.Dualof p (T.Var p' y))
    | y == x    = dualof t
    | otherwise = u
  subs _ _ t            = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Validation.Substitution.subs" t


instance (Subs T.Type Variable t) => Subs T.Type Variable (Bind k t) where
  subs t x (Bind p y k u) = Bind p y k (subs t x u)

-- CoVar subs, [t/co-x]u

class Cosubs t where
  cosubs :: T.Type -> Variable -> t -> t

instance Cosubs T.Type where
  -- Functional types
  cosubs t x (T.Message p pol t1) = T.Message p pol (cosubs t x t1)
  cosubs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (cosubs t x t1) (cosubs t x t2)
  -- Session types
  cosubs t x (T.Semi   p t1 t2  ) = T.Semi p (cosubs t x t1) (cosubs t x t2)
  cosubs t x (T.Almanac p s  m   ) = T.Almanac p s (Map.map (cosubs t x) m)
    -- Polymorphism and recursion
  cosubs t x (T.Rec    p b      ) = T.Rec p (cosubs t x b)
  cosubs t x (T.Forall p b      ) = T.Forall p (cosubs t x b)
  cosubs t x u@(T.Dualof _ (T.Var _ y))
    | y == x = t
    | otherwise = u
  cosubs _ _ t            = t


instance Cosubs t => Cosubs (Bind K.Kind t) where
  cosubs t x (Bind p y k u) = Bind p y k (cosubs t x u)


-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold t@(T.Rec _ (Bind _ x _ u)) = subs t x u
unfold t = internalError "Validation.Substitution.unfold" t


{-

-- Not needed. Cf. Validation.Renam.isFreeIn.
-- The set of free type variables in a type
free :: T.Type -> Set.Set Variable
  -- Functional types
free (T.Arrow _ _ t u) = free t `Set.union` free u
free (T.Pair _ t u) = free t `Set.union` free u
free (T.Variant _ m) = freeMap m
  -- Session types
free (T.Semi _ t u) = free t `Set.union` free u
free (T.Choice _ _ m) = freeMap m
  -- Functional or session
free (T.Rec _ (Bind _ x _) t) = Set.delete x (free t)
free (T.Var _ x) = Set.singleton x
  -- T.Type operators
free t@T.Dualof{} = internalError "Validation.Substitution.free" t
  -- Otherwise: Basic, Skip, Message
free _ = Set.empty

freeMap :: T.TypeMap -> Set.Set Variable
freeMap = Map.foldr (\t acc -> free t `Set.union` acc) Set.empty

Define [t/x]u to be the result of substituting t for every free
occurrence of x in u, and changing bound variables to avoid clashes
[Hindley&Seldin 1986, Definition 1.11]

Does not work with bisimilarity, for substitution does not preserve
the is-renamed predicate.

subs :: T.Type -> Variable -> T.Type -> T.Type
  -- Functional types
subs t x (Fun p m u v)    = Fun p m (subs t x u) (subs t x v)
subs t x (Pair p u v) = Pair p (subs t x u) (subs t x v)
subs t x (Datatype p m)   = Datatype p (Map.map (subs t x) m)
  -- Session types
subs t x (Semi p u v)     = Semi p (subs t x u) (subs t x v)
subs t x (Choice p v m)   = Choice p v (Map.map (subs t x) m)
subs t x u@(Rec p yk@(Bind q y k) v)
  | y == x                = u
  -- | y `Set.notMember` (free t) || x `Set.notMember` (free v) = Rec p yk (subs t x v)
  | otherwise             = Rec p (Bind q z k) (subs t x (subs (Variable q z) y v))
    where z = mkNewVar 0 y
  -- Functional or session
subs t x u@(Variable _ y)
  | y == x                = t
  | otherwise             = u
  -- T.Type operators  
subs t x (Dualof p u)     = Dualof p (subs t x u)
  -- Otherwise: Basic, Skip, Message, T.TypeName
subs _ _ t                = t

-}
