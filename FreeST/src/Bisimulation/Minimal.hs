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
  , minimalF
  , firstF
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

minimal :: Type -> Type
minimal t = trace ("# N[" ++ show t ++ "] => " ++ show t') t'
  where t' = minimalF cleanFresh t

minimalF :: [Int] -> Type -> Type
-- f: list of possible fresh variables
  -- Functional Types
minimalF f (Arrow s m t u) = Arrow s m (minimalF f t) (minimalF f u)
minimalF f (Labelled s k m) = Labelled s k (Map.map (minimalF f) m)
-- session types
minimalF f (Message s p t) = Message s p (minimalF f t)
minimalF f (Semi s t u) = Semi s (minimalF f' t) (minimalF f u)
  where f' = filter (`notElem` freei u) f
  -- Polymorphism and recursive types
minimalF f t@(Forall s1 (Bind s2 a k u)) =
  Forall s1 (Bind s2 b k (minimalF f (subs vb a u)))
    where ai = firstF f u
          --b = Variable (getSpan a) ("#" ++ extern a) ai
          b = Variable (getSpan a) ("#" ++ show ai) ai
          -- TODO: perguntar prof. vasco
          vb = Var (getSpan b) b
minimalF f (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimalF f t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimalF f t
  -- Type operators
minimalF f (Dualof s t) = Dualof s (minimalF f t)
  -- Int, Float, Char, String, Skip, End, Var
minimalF _ t = t


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

cleanFresh = [1..]

first = firstF cleanFresh

-- assumes list is ordered asc.
firstF :: [Int] -> Type -> Int
firstF fresh t = f
  where f = head $ filter (`notElem` freei t) fresh

freei :: Type -> [Int]
freei t = Set.toList $ Set.map (\(Variable _ _ n) -> n) $ Set.filter (\(Variable _ id _) -> head id == '#') (free t)


