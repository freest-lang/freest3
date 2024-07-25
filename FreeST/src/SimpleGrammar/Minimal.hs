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
minimal' set (Arrow s m t u) =
  Arrow s m (minimal' set t) (minimal' set u)
  -- Arrow s m (minimal' (set `Set.union` used u) t) (minimal' set u)
  -- We may not need the union here Functional and Session Types
minimal' set (Labelled s k m) =
  Labelled s k (Map.map (minimal' set) m)
  -- Session Types
minimal' set (Semi s t u) =
  Semi s (minimal' (set `Set.union` used u) t) (minimal' set u)
minimal' set (Message s p t) =
  Message s p (minimal' set t)
  -- Polymorphism and recursive types
minimal' set t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimal' set (subs vb a u)))
    where b = first (set `Set.union` used t) -- mkNewVar (first t) a
          vb = Var (getSpan b) b
minimal' set (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimal' set t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimal' set t
  -- Type operators
minimal' set (Dualof s t) =
  Dualof s (minimal' set t)
  -- Int, Float, Char, String, Skip, End, Var
minimal' _ t = t

first :: Set.Set Variable -> Variable
first set = head $ filter (`Set.notMember` set) freshVars
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

-- The set of type variables used in a type (similar to free, except in case Semi
used :: Type -> Set.Set Variable
  -- Functional Types
used (Arrow _ _ t u) = used t `Set.union` used u
used (Labelled _ _ m) =
  Map.foldr (\t acc -> used t `Set.union` acc) Set.empty m
  -- Session Types
used (Semi _ t u)
  | normed Set.empty t = used t `Set.union` used u
  | otherwise = used t
used (Message _ _ t) = used t 
  -- Polymorphism and recursive types
used (Forall _ (Bind _ a _ t)) = Set.delete a (used t)
used (Rec _ (Bind _ a _ t)) = Set.delete a (used t)
used (Var _ x) = Set.singleton x
  -- Type operators
used (Dualof _ t) = used t
  --Int, Float, Char, String, Skip, End
used _ = Set.empty 
