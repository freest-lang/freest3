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
import           Syntax.Type hiding (free)
import           Typing.Substitution ( subs )
import           Kinding.Norm        ( normed )

import qualified Data.Map.Strict     as Map
import qualified Data.Set as Set

minimal :: Type -> Type
minimal = minimal' Set.empty

minimal' :: Set.Set Variable -> Type -> Type
  -- Functional Types
minimal' used (Arrow s m t u) = Arrow s m (minimal' (used `Set.union` free u) t) (minimal' used u) -- We may not need the union here
  -- Functional and Session Types
minimal' used (Labelled s k m) = Labelled s k (Map.map (minimal' used) m)
  -- Session Types
-- minimal' used (Semi s t u) = Semi s (minimal' (used `Set.union` free u) t) (minimal' used u)
minimal' used (Semi s t u) = Semi s (minimal' used' t) (minimal' used u)
  where used' = if normed Set.empty t then used `Set.union` free u else used
minimal' used (Message s p t) = Message s p (minimal' used t)
  -- Polymorphism and recursive types
minimal' used t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimal' used (subs vb a u)))
    where b = first (used `Set.union` free t) -- mkNewVar (first t) a
          vb = Var (getSpan b) b
minimal' used (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimal' used t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimal' used t
  -- Type operators
minimal' used (Dualof s t) = Dualof s (minimal' used t)
  -- Int, Float, Char, String, Skip, End, Var
minimal' _ t = t

first :: Set.Set Variable -> Variable
first used = head $ filter (`Set.notMember` used) freshVars
  where
    -- ["%1", "%2", ...]
    freshVars = [mkVar defaultSpan $ "%" ++ show i | i <- [1..]]

-- first :: Type -> Int
-- first t = first' 0 (Set.toList (Set.map (\(Variable _ _ n) -> n) (free t)))
--   -- TODO: avoid converting to list
--   where
--     first' :: Int -> [Int] -> Int
--     first' n [] = n
--     first' n (m:ms)
--       | n < m     = n
--       | otherwise = first' (n + 1) ms

-- The set of free type variables in a type
free :: Type -> Set.Set Variable
  -- Functional Types
free (Arrow _ _ t u) = free t `Set.union` free u
free (Labelled _ _ m) =
  Map.foldr (\t acc -> free t `Set.union` acc) Set.empty m
  -- Session Types
free (Semi _ t u) = free t `Set.union` free u
free (Message _ _ t) = free t 
  -- Polymorphism and recursive types
free (Forall _ (Bind _ a _ t)) = Set.delete a (free t)
free (Rec _ (Bind _ a _ t)) = Set.delete a (free t)
free (Var _ x) = Set.singleton x
  -- Type operators
free (Dualof _ t) = free t
  --Int, Float, Char, String, Skip, End
free _ = Set.empty 
