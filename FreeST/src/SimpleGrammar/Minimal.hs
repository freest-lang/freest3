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

module SimpleGrammar.Minimal 
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
minimal = minimal' Set.empty

minimal' :: Set.Set Variable -> Type -> Type
  -- Functional Types
minimal' fv (Arrow s m t u) = Arrow s m (minimal' (fv `Set.union` free u) t) (minimal' fv u)
  -- Functional and Session Types
minimal' fv (Labelled s k m) = Labelled s k (Map.map (minimal' fv) m)
  -- Session Types
minimal' fv (Semi s t u) = Semi s (minimal' (fv `Set.union` free u) t) (minimal' fv u)
minimal' fv (Message s p t) = Message s p (minimal' fv t)
  -- Polymorphism and recursive types
minimal' fv t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimal' fv (subs vb a u)))
    where b = first fv t -- mkNewVar (first t) a
          vb = Var (getSpan b) b
minimal' fv (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimal' fv t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimal' fv t
  -- Type operators
minimal' fv (Dualof s t) = Dualof s (minimal' fv t)
  -- Int, Float, Char, String, Skip, End, Var
minimal' _ t = t

first :: Set.Set Variable -> Type -> Variable
first fv t = head $ filter (`Set.notMember` (fv `Set.union` free t)) freshVars
  where
    -- ["#1", "#2", ...]
    freshVars = map (\i -> mkVar defaultSpan $ "#" ++ show i) [1..]

-- first :: Type -> Int
-- first t = first' 0 (Set.toList (Set.map (\(Variable _ _ n) -> n) (free t)))
--   -- TODO: avoid converting to list
--   where
--     first' :: Int -> [Int] -> Int
--     first' n [] = n
--     first' n (m:ms)
--       | n < m     = n
--       | otherwise = first' (n + 1) ms
