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

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import qualified Validation.Rename             as Rename
                                                ( subs
                                                , unfold
                                                )
import qualified Validation.Substitution       as Subs
                                                ( subs
                                                , unfold
                                                )
import           Bisimulation.Grammar
import           Bisimulation.Bisimulation     as Bisimulation
import           Equivalence.TypeToGrammar
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Debug.Trace --debug

class Equivalence t where
  equivalent :: TypeEnv -> KindEnv -> t -> t -> Bool

-- Types

type Visited = Set.Set (Type, Type)

-- A co-inductive definition for functional types. A bisimulation
-- based definition for session types
instance Equivalence Type where
  equivalent tenv kenv = equiv Set.empty
   where
    equiv :: Visited -> Type -> Type -> Bool
    -- Have we been here before?
    equiv v t u | (t, u) `Set.member` v = True
    -- Functional types
    equiv _ (IntType _) (IntType _)     = True
    equiv _ (CharType _) (CharType _)   = True
    equiv _ (BoolType _) (BoolType _)   = True
    equiv _ (UnitType _) (UnitType _)   = True
    equiv v (Fun _ m t1 t2) (Fun _ n u1 u2) =
      m == n && equiv v t1 u1 && equiv v t2 u2
    equiv v (PairType _ t1 t2) (PairType _ u1 u2) =
      equiv v t1 u1 && equiv v t2 u2
    equiv v (Datatype _ m1) (Datatype _ m2) =
      Map.size m1 == Map.size m2 && Map.foldlWithKey (equivField v m2) True m1
    -- Polymorphism
    equiv v (Forall _ kb1 t) (Forall _ kb2 u) = -- TODO: check kindbinds ?
      -- kb1 == kb2 &&
      equiv (Set.insert (t, u) v) t u
    -- Recursion
    equiv _ (TypeVar _ x) (TypeVar _ y) = x == y -- A free (a polymorphic) type var
    equiv v t@Rec{} u = equiv (Set.insert (t, u) v) (Subs.unfold t) u
    equiv v t u@Rec{} = equiv (Set.insert (t, u) v) t (Subs.unfold u)
    -- Type operators
    equiv _ (TypeName _ x) (TypeName _ y) = -- trace ("TNAME 1 " ++ show x) $
      x == y -- Admissible
    equiv v (TypeName _ x) u = -- trace ("TNAME 2 " ++ show x) $
      equiv v (getType x) u
    equiv v t (TypeName _ y) = -- trace ("TNAME 3 " ++ show y) $
      equiv v t (getType y)
    -- Session types
    equiv _ t u =
      isSessionType tenv kenv t
        && isSessionType tenv kenv u
        && Equivalence.Equivalence.bisimilar tenv t u

    equivField :: Visited -> TypeMap -> Bool -> ProgVar -> Type -> Bool
    equivField v m acc l t = acc && l `Map.member` m && equiv v (m Map.! l) t

    getType :: TypeVar -> Type
    getType x = snd $ tenv Map.! x

bisimilar :: TypeEnv -> Type -> Type -> Bool
bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]
  -- where Grammar [xs, ys] p = convertToGrammar tEnv [t, u]
    -- trace (show t ++ "\nbisim\n" ++ show u) $ convertToGrammar tEnv [t, u]

-- Assumes the type is well formed
isSessionType :: TypeEnv -> KindEnv -> Type -> Bool
  -- Session types
isSessionType _ _ (Skip _) = True
isSessionType _ _ Semi{} = True
isSessionType _ _ Message{} = True
isSessionType _ _ Choice{} = True
  -- Recursion
isSessionType _ _ (Rec _ (KindBind _ _ k) _) = isSession k
isSessionType _ kenv (TypeVar _ x) = Map.member x kenv
  -- Type operators
isSessionType _ _ Dualof{} = True
isSessionType tenv _ (TypeName _ x) = isSession $ fst $ tenv Map.! x
  -- Otherwise: Functional types
isSessionType _    _    _ = False

-- Type schemes

-- instance Equivalence TypeScheme where
--   equivalent tenv kenv1 ts1 ts2 = case instantiate ts1 ts2 of
--     Nothing              -> False
--     Just (kenv2, t1, t2) -> equivalent tenv (kenv1 `Map.union` kenv2) t1 t2

-- instantiate :: TypeScheme -> TypeScheme -> Maybe (KindEnv, Type, Type)
-- instantiate (TypeScheme _ bs1 t1) (TypeScheme _ bs2 t2) = inst bs1 bs2 t1 t2
--  where
--   inst
--     :: [KindBind]
--     -> [KindBind]
--     -> Type
--     -> Type
--     -> Maybe (KindEnv, Type, Type)
--   inst (KindBind p1 x1 k1 : bs1) (KindBind _ x2 k2 : bs2) t1 t2
--     | k1 /= k2 = Nothing
--     | otherwise = -- substitute x1 for x2 in t2
--                   fmap (\(m, t1', t2') -> (Map.insert x1 k1 m, t1', t2'))
--                        (inst bs1 bs2 t1 (Rename.subs (TypeVar p1 x1) x2 t2))
--   inst [] [] t1 t2 = Just (Map.empty, t1, t2)
--   inst _  _  _  _  = Nothing

-- Typing environments

instance Equivalence VarEnv where
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
