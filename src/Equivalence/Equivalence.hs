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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Equivalence.Equivalence
( Equivalence(..)
) where

import           Syntax.Programs
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Validation.Kinding
import           Equivalence.Normalisation
import qualified Equivalence.Bisimulation as Bisimulation
import qualified Data.Map.Strict as Map

class Equivalence t where
  equivalent :: TypeEnv -> KindEnv -> t -> t -> Bool

-- Types

instance Equivalence Type where
   equivalent tenv kenv t u = normalise tenv t == normalise tenv u || equiv kenv t u
  -- equivalent tenv kenv t u = t == u || equiv kenv t u
  -- equivalent tenv kenv t u = equiv kenv t u
    where
    equiv :: KindEnv -> Type -> Type -> Bool
      -- Functional types
    equiv _ (Basic _ b) (Basic _ c) = b == c
    equiv kenv (Fun _ m t1 t2) (Fun _ n u1 u2) =
      m == n && equiv kenv t1 u1 && equiv kenv t2 u2
    equiv kenv (PairType _ t1 t2) (PairType _ u1 u2) =
      equiv kenv t1 u1 && equiv kenv t2 u2
    equiv kenv (Datatype _ m1) (Datatype _ m2) =
      Map.size m1 == Map.size m2 &&
      Map.foldlWithKey (checkConstructor kenv m2) True m1
      -- Functional or session
    equiv _ (TypeVar _ x) (TypeVar _ y) = x == y
      -- Type operatorstre  
    equiv kenv (Dualof _ t) u = equiv kenv (dual t) u
    equiv kenv t (Dualof _ u) = equiv kenv t (dual u)
    equiv _ (Name _ c1) (Name _ c2) = c1 == c2 -- TODO: this works for datatypes but not for type declarations, where one has to expand the definition(s) for c1 (c2) and continue
    -- TODO: THIS CAN EASILY LOOP
    equiv kenv (Name p c) u = equiv kenv t u
      where (_, TypeScheme _ [] t) = tenv Map.! (TBind p c) -- TODO: polymorphic type names
    equiv kenv t (Name p c) = equiv kenv t u
      where (_, TypeScheme _ [] u) = tenv Map.! (TBind p c) -- TODO: polymorphic type names
      -- Session types
    equiv kenv t u =
      isSessionType tenv kenv t &&
      isSessionType tenv kenv u &&
      Bisimulation.equivalent t u
    
    checkConstructor :: KindEnv -> TypeMap -> Bool -> PBind -> Type -> Bool
    checkConstructor kenv m acc l t =
      acc && l `Map.member` m && equiv kenv (m Map.! l) t

-- Type schemes

instance Equivalence TypeScheme where
  equivalent tenv kenv1 ts1 ts2 =
    case instantiate ts1 ts2 of
      Nothing             -> False
      Just (kenv2, t1, t2) -> equivalent tenv (kenv1 `Map.union` kenv2) t1 t2

instantiate :: TypeScheme -> TypeScheme -> Maybe (KindEnv, Type, Type)
instantiate (TypeScheme _ bs1 t1) (TypeScheme _ bs2 t2) = inst bs1 bs2 t1 t2
  where
  inst :: [TBindK] -> [TBindK] -> Type -> Type -> Maybe (KindEnv, Type, Type)
  inst ((TBindK p1 x1 k1):bs1) (tk2@(TBindK _ x2 k2):bs2) t1 t2
    | k1 /= k2  = Nothing
    | x1 == x2 = inst bs1 bs2 t1 (subs (TypeVar p1 x1) tk2 t2)
    | otherwise = -- substitute x1 for x2
        case inst bs1 bs2 t1 (subs (TypeVar p1 x1) tk2 t2) of
          Nothing -> Nothing
          Just (m, t1, t2) -> Just (Map.insert (TBind p1 x1) k1 m, t1, t2)
  inst [] [] t1 t2 = Just (Map.empty, t1, t2)
  inst _ _ _ _ = Nothing

-- Typing environments

instance Equivalence VarEnv where
  equivalent tenv kenv env1 env2 =
    Map.size env1 == Map.size env2 &&
    Map.foldlWithKey (\acc b s -> acc && b `Map.member` env2 && equivalent tenv kenv s (env2 Map.! b)) True env1
