{- |
Module      :  Position
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Unfolding of recursive types and substitution ([u/x]t, substitute u for x on t)
-}

module Validation.Substitution
( subs
, unfold
) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Validation.Rename
import qualified Data.Map.Strict as Map

unfold :: Type -> Type
unfold t@(Rec _ (TypeVarBind _ x _) u) = subs t x u

-- [u/x]t, substitute u for x in t
subs :: Type -> TypeVar -> Type -> Type
subs t1 x t2 = renameType $ sub t1 x t2
  -- Functional types
sub t x (Fun p m t1 t2)    = Fun p m (sub t x t1) (sub t x t2)
sub t x (PairType p t1 t2) = PairType p (sub t x t1) (sub t x t2)
sub t x (Datatype p m)     = Datatype p (Map.map(sub t x) m)
  -- Session types
sub t x (Semi p t1 t2)     = Semi p (sub t x t1) (sub t x t2)
sub t x (Choice p v m)     = Choice p v (Map.map(sub t x) m)
sub t x (Rec p yk u)       = Rec p yk (sub t x u) -- Assume types were renamed (hence, x/=y and no on-the-fly renaming needed)
-- sub t x (Rec p b@(TypeVarBind _ y _) u)
--   | y == x                  = u
--   | otherwise               = Rec p b (sub t x u)
  -- Functional or session
sub t x u@(TypeVar _ y)
  | y == x                  = t
  | otherwise               = u
  -- Type operators  
sub t x (Dualof p u)       = Dualof p (sub t x u)
  -- Otherwise: Basic, Skip, Message, TypeName
sub _ _ t                  = t

