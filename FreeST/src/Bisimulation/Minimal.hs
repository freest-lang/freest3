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
import           Typing.Normalisation ( normalise )


import           Data.List (union)

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

--minimal :: Type -> Type
--  -- Functional Types
--minimal (Arrow s m t u) = Arrow s m (minimal t) (minimal u)
--minimal (Labelled s k m) = Labelled s k (Map.map minimal m)
--  -- Session Types
--minimal (Semi s (Forall sf (Bind sb a k t)) u) = Forall s (Bind sb a k (minimal (subs vb a (Semi s t u))))
--  where b = mkNewVar (first t) a
--        vb = Var (getSpan b) b
--minimal v@(Semi s t u)
--  | whnf v = Semi s (minimal t) (minimal u)
--  | otherwise = minimal (normalise v)
--  -- lint 
--minimal (Message s p t) = Message s p (minimal t)
--  -- Polymorphism and recursive types
--minimal t@(Forall s1 (Bind s2 a k u)) =
--  Forall s1 (Bind s2 b k (minimal (subs vb a u)))
--    where b = mkNewVar (first t) a
--          vb = Var (getSpan b) b
--minimal (Rec s1 (Bind s2 a k t))
--  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimal t))
--  -- Required by the current translation to grammar. Otherwise:
--  -- uncaught exception: PatternMatchFail
--  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
--  | otherwise = minimal t
--  -- Type operators
--minimal (Dualof s t) = Dualof s (minimal t)
--  -- Int, Float, Char, String, Skip, End, Var
--minimal t = t

minimal = minimalS cleanFresh

minimalS :: [Int] -> Type -> Type
  -- Functional Types
minimalS fresh (Arrow s m t u) = Arrow s m (minimalS fresh t) (minimalS fresh u)
minimalS fresh (Labelled s k m) = Labelled s k (Map.map (minimalS fresh) m)
-- session types
minimalS fresh (Message s p t) = Message s p (minimalS fresh t)
minimalS fresh (Semi s t u) = Semi s (minimalS fresh' t) (minimalS fresh u)
  where fresh' = fresh `union` freei u
  -- Polymorphism and recursive types
minimalS fresh t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimalS fresh (subs vb a u)))
    where ai = firstS fresh u
          b = mkNewVar ai a  -- to keep original var meta
          vb = Var (getSpan b) b
minimalS fresh (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimalS fresh t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimalS fresh t
  -- Type operators
minimalS fresh (Dualof s t) = Dualof s (minimalS fresh t)
  -- Int, Float, Char, String, Skip, End, Var
minimalS _ t = t


-- precondition: dualof is only applied to T.Var at this point
--whnf :: Type -> Bool
--whnf Rec{} = False
--whnf (Semi _ Skip{} _)     = False
--whnf (Semi _ End{} _)      = False
--whnf (Semi _ Labelled{} _) = False
--whnf (Semi _ Semi{} _)     = False
--whnf (Semi _ Rec{} _)      = False
--whnf _                     = True

--first :: Type -> Int
--first t = first' 0 $ Set.toList $ Set.map (\(Variable _ _ n) -> n) (free t)
--  -- TODO: avoid converting to list
--  where
--    first' :: Int -> [Int] -> Int
--    first' n [] = n
--    first' n (m:ms)
--      | n < m     = n
--      | otherwise = first' (n + 1) ms



-- OBS: fresh variables can't be implemented with sets, as Data.Set doesn't support infinite sets. It hangs.

--cleanFresh :: Set.Set Variable
--cleanFresh = Set.fromAscList $ map (\i -> Variable defaultSpan ("#" ++ show i) i) [1..]

--first :: Type -> Variable
--first = firstS cleanFresh

--firstS :: Set.Set Variable -> Type -> Variable
--firstS fresh t = head $ filter (`Set.notMember` free t) (Set.toList fresh)

cleanFresh = [1..]

first = firstS cleanFresh

-- assumes list is ordered asc.
firstS :: [Int] -> Type -> Int
firstS fresh t = head $ filter (`notElem` freei t) fresh

freei :: Type -> [Int]
freei t = Set.toList $ Set.map (\(Variable _ _ n) -> n) (free t)


