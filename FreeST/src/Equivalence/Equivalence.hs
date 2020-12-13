{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE FlexibleInstances #-}

module Equivalence.Equivalence
  ( Equivalence(..)
  , Equivalence.Equivalence.bisimilar -- for session types only, for testing purposes
  )
where

import           Bisimulation.Bisimulation     as Bisimulation
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Equivalence.TypeToGrammar
import           Syntax.Base
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import qualified Validation.Substitution       as Subs
                                                ( unfold )


class Equivalence t where
  equivalent :: T.TypeEnv -> K.KindEnv -> t -> t -> Bool

-- Types

-- We removed the Ord instance. Used to be: 
-- type Visited = Set.Set (T.Type, T.Type)
type Visited = Set.Set (Pos, Pos)

-- A co-inductive definition for functional types. A bisimulation
-- based definition for session types
instance Equivalence T.Type where
  equivalent tenv kenv = equiv Set.empty
   where
    equiv :: Visited -> T.Type -> T.Type -> Bool
    -- Have we been here before?
    equiv v t u | (pos t, pos u) `Set.member` v = True
    -- Functional types
    equiv _ (T.Int  _) (T.Int  _)               = True
    equiv _ (T.Char _) (T.Char _)               = True
    equiv _ (T.Bool _) (T.Bool _)               = True
    equiv _ (T.Unit _) (T.Unit _)               = True
    equiv v (T.Fun _ m t1 t2) (T.Fun _ n u1 u2) =
      m == n && equiv v t1 u1 && equiv v t2 u2
    equiv v (T.Pair _ t1 t2) (T.Pair _ u1 u2) = equiv v t1 u1 && equiv v t2 u2
    equiv v (T.Datatype _ m1) (T.Datatype _ m2) =
      Map.size m1 == Map.size m2 && Map.foldlWithKey (equivField v m2) True m1
    -- Polymorphism
    equiv v (T.Forall _ _ t) (T.Forall _ _ u) = -- TODO: check kindbinds ?
      -- kb1 == kb2 &&
      equiv (Set.insert (pos t, pos u) v) t u
    -- Recursion
    equiv _ (T.Var _ x) (T.Var _ y) = x == y -- A free (a polymorphic) type var
    equiv v t@T.Rec{} u = equiv (Set.insert (pos t, pos u) v) (Subs.unfold t) u
    equiv v t u@T.Rec{} = equiv (Set.insert (pos t, pos u) v) t (Subs.unfold u)
    -- Type operators
    equiv _ (T.Name _ x) (T.Name _ y) = -- trace ("TNAME 1 " ++ show x) $
      x == y -- Admissible
    equiv v (T.Name _ x) u = -- trace ("TNAME 2 " ++ show x) $
      equiv v (getType x) u
    equiv v t (T.Name _ y) = -- trace ("TNAME 3 " ++ show y) $
      equiv v t (getType y)
    -- Session types
    equiv _ t u =
      isSessionType tenv kenv t
        && isSessionType tenv kenv u
        && Equivalence.Equivalence.bisimilar tenv t u

    equivField :: Visited -> T.TypeMap -> Bool -> ProgVar -> T.Type -> Bool
    equivField v m acc l t = acc && l `Map.member` m && equiv v (m Map.! l) t

    getType :: TypeVar -> T.Type
    getType x = snd $ tenv Map.! x

bisimilar :: T.TypeEnv -> T.Type -> T.Type -> Bool
bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]
  -- where Grammar [xs, ys] p = convertToGrammar tEnv [t, u]
    -- trace (show t ++ "\nbisim\n" ++ show u) $ convertToGrammar tEnv [t, u]

-- Assumes the type is well formed
isSessionType :: T.TypeEnv -> K.KindEnv -> T.Type -> Bool
  -- Session types
isSessionType _    _    (T.Skip _)                 = True
isSessionType _    _    T.Semi{}                   = True
isSessionType _    _    T.Message{}                = True
isSessionType _    _    T.Choice{}                 = True
  -- Recursion
isSessionType _    _    (T.Rec _ (K.Bind _ _ k) _) = K.isSession k
isSessionType _    kenv (T.Var _ x               ) = Map.member x kenv
  -- Type operators
isSessionType _    _    T.Dualof{}                 = True
isSessionType tenv _ (T.Name _ x) = K.isSession $ fst $ tenv Map.! x
  -- Otherwise: Functional types
isSessionType _    _    _                          = False

-- Typing environments

instance Equivalence T.VarEnv where
  equivalent tenv kenv env1 env2 =
    Map.size env1
      == Map.size env2
      && Map.foldlWithKey
           (\acc b s -> acc && b `Map.member` env2 && equivalent
             tenv
             kenv
             s
             (env2 Map.! b)
           )
           True
           env1
