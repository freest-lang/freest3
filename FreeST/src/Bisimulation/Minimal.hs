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


import           Data.List (union, delete)

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           Debug.Trace

minimal = minimalS cleanFresh

minimalS :: [Int] -> Type -> Type
-- f: set of available fresh variables
  -- Functional Types
minimalS f (Arrow s m t u) = Arrow s m (minimalS f t) (minimalS f u)
minimalS f (Labelled s k m) = Labelled s k (Map.map (minimalS f) m)
-- session types
minimalS f (Message s p t) = Message s p (minimalS f t)
minimalS f (Semi s t u) = Semi s (minimalS f' t) (minimalS f u)
  where f' = filter (`notElem` freei u) f
  -- Polymorphism and recursive types
minimalS f t@(Forall s1 (Bind s2 a k u)) =
  --trace "# inside forall" (Forall s1 (Bind s2 b k (minimalS f (subs vb a u))))
  Forall s1 (Bind s2 b k (minimalS f (subs vb a u)))
    where ai = firstS f u
          --b = mkNewVar ai a  -- to keep original var meta
          b = Variable (getSpan a) ("#" ++ extern a) ai
          vb = Var (getSpan b) b
minimalS f (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimalS f t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimalS f t
  -- Type operators
minimalS f (Dualof s t) = Dualof s (minimalS f t)
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
firstS fresh t = trace ("# Chosen var: " ++ show f ++ ". Type: \"" ++ show t ++ "\"") f
  where f = head $ filter (`notElem` freei t) fresh

freei :: Type -> [Int]
--freei t = trace ("# Free vars in \"" ++ show t ++ "\": " ++ show fv) fv
freei t = fv
  where fv = Set.toList $ Set.map (\(Variable _ _ n) -> n) $ Set.filter (\(Variable _ id _) -> head id == '#')(free t)


