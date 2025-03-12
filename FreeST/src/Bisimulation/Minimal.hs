{- |
Module      :  Bisimulation.Minimal
Description :  Minimal renaming
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  stable
Portability :  portable

Renames the polymorphic variables of a type by choosing the first variable not
free in the type.

Additionally transforms types of the form `rec a:k.T` in `T` when x is not free
in `T`. This facilitates translation into grammars.
-}

module Bisimulation.Minimal 
  ( minimal
  , first
  )
where

import           Syntax.Base
import           Syntax.Type
import           Typing.Substitution ( subs )

import qualified Data.Map.Strict     as Map
import qualified Data.Set as Set

minimal :: Type -> Type
  -- Functional Types
minimal (Arrow s m (l1,l2) t u) = Arrow s m (l1,l2) (minimal t) (minimal u)
minimal (Labelled s k m) = Labelled s k (Map.map minimal m)
  -- Session Types
minimal (Semi s t u) = Semi s (minimal t) (minimal u)
minimal (Message s l p t) = Message s l p (minimal t)
  -- Polymorphism and recursive types
minimal t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimal (subs vb a u)))
    where b = mkNewVar (first t) a
          vb = Var (getSpan b) b
minimal (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimal t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimal t
  -- Type operators
minimal (Dualof s t) = Dualof s (minimal t)
  -- Int, Float, Char, String, Skip, End, Var
minimal t = t

first :: Type -> Int
first t = first' 0 (Set.toList (Set.map (\(Variable _ _ n) -> n) (free t)))
  -- TODO: avoid converting to list
  where
    first' :: Int -> [Int] -> Int
    first' n [] = n
    first' n (m:ms)
      | n < m     = n
      | otherwise = first' (n + 1) ms
