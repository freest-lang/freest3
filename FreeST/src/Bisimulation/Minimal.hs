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

To allow debug printing, set the constant "minimalDebug" to true. This will
rename the variable names to their fresh variable number.
-}

module Bisimulation.Minimal
  ( minimal
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
import Text.ParserCombinators.ReadPrec (reset)
import           Text.Printf


-- This variable should be accessible as an option in the terminal. To allow
-- easier debugging
minimalDebug = True

minimal :: Type -> Type
minimal t = if minimalDebug
  then trace ("# N[" ++ show t ++ "] => " ++ show t') t'
  else t'
    where t' = minimalA freshIDs t

minimalA :: [Int] -> Type -> Type
-- f: list of possible fresh variables
  -- Functional Types
minimalA f (Arrow s m t u) = Arrow s m (minimalA f t) (minimalA f u)
minimalA f (Labelled s k m) = Labelled s k (Map.map (minimalA f) m)
-- session types
minimalA f (Message s p t) = Message s p (minimalA f t)
minimalA f (Semi s t u) = Semi s (minimalA f' t) (minimalA f u)
  where f' = filter (`notElem` freei u) f
  -- Polymorphism and recursive types
--minimalA f t@(Forall s1 (Bind s2 a k u)) = trace (printf "  # %s => %s (#%d)" (vardb a) (vardb b) ai) res
minimalA f t@(Forall s1 (Bind s2 a k u)) = res
  where res = Forall s1 (Bind s2 b k (minimalA f (subs vb a u)))
        ai = first f t
        b = if minimalDebug
          then Variable (getSpan a) ("#" ++ show ai) ai
          else mkNewVar ai a
        vb = Var (getSpan b) b
minimalA f (Rec s1 (Bind s2 a k t))
  | a `isFreeIn` t = Rec s1 (Bind s2 a k (minimalA f t))
  -- Required by the current translation to grammar. Otherwise:
  -- uncaught exception: PatternMatchFail
  --  src/Bisimulation/TypeToGrammar.hs:130:3-39: Non-exhaustive patterns in z : zs  
  | otherwise = minimalA f t
  -- Type operators
minimalA f (Dualof s t) = Dualof s (minimalA f t)
  -- Int, Float, Char, String, Skip, End, Var
--minimalA f t@(Var s v) = trace ("- found Variable: " ++ vardb v) t
minimalA _ t = t


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
freshIDs :: [Int]
freshIDs = [-2,-3..]
-- to avoid conflict between recursive and polymorphic variables indices. Polymorphic variables should start at -2 and decrease. -1 is default value in base.hs
-- why is this needed?? whats messing with the positive values?


-- assumes list is ordered asc.
first :: [Int] -> Type -> Int
first freshIDs t = f
  where f = head $ filter (`notElem` freei t) freshIDs

freei :: Type -> [Int]
freei t = Set.toList $ Set.map (\(Variable _ _ n) -> n) (free t)



vardb v = extern v ++ " " ++ intern v