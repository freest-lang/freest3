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
, isSessionType
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import qualified Validation.Rename as Rename (subs)
import           Equivalence.Normalisation
import           Equivalence.Bisimulation
import qualified Data.Map.Strict as Map

class Equivalence t where
  equivalent :: TypeEnv -> KindEnv -> t -> t -> Bool

-- Types

instance Equivalence Type where
  -- equivalent tenv kenv t u = normalise tenv t == normalise tenv u || equiv t u
  equivalent tenv kenv t u = t == u || equiv t u
    where
    equiv :: Type -> Type -> Bool
      -- Functional types
    equiv (Basic _ b) (Basic _ c) = b == c
    equiv (Fun _ m t1 t2) (Fun _ n u1 u2) =
      m == n && equiv t1 u1 && equiv t2 u2
    equiv (PairType _ t1 t2) (PairType _ u1 u2) =
      equiv t1 u1 && equiv t2 u2
    equiv (Datatype _ m1) (Datatype _ m2) =
      Map.size m1 == Map.size m2 &&
      Map.foldlWithKey (equivField m2) True m1 -- TODO: Use all
      -- Functional or session
    equiv (TypeVar _ x) (TypeVar _ y) = x == y -- A free type var
    -- equiv t@(Rec _ _ _) u = equiv (unfold t) u -- TODO: recipe for looping?
    -- equiv t u@(Rec _ _ _) = equiv t (unfold u) -- TODO: recipe for looping?
      -- Type operators
    -- equiv (Dualof _ (TypeName _ x)) u = equiv (dual (getType x)) u -- These are session types
    -- equiv t (Dualof _ (TypeName _ y)) = equiv t (dual (getType y))
    -- equiv (Dualof _ t) u = equiv (dual t) u
    -- equiv t (Dualof _ u) = equiv t (dual u)
    equiv (TypeName _ x) (TypeName _ y) = x == y -- TODO: x may be diff from y and yet the types be equiv
    equiv (TypeName _ x) u = equiv (getType x) u
    equiv t (TypeName _ y) = equiv t (getType y)
      -- Session types
    equiv t u =
      isSessionType tenv kenv t &&
      isSessionType tenv kenv u &&
      bisimilar tenv t u

    equivField :: TypeMap -> Bool -> ProgVar -> Type -> Bool
    equivField m acc l t = acc && l `Map.member` m && equiv (m Map.! l) t

    getType :: TypeVar -> Type
    getType x = toType (snd (tenv Map.! x))

-- Assumes the type is well formed
isSessionType :: TypeEnv -> KindEnv -> Type -> Bool
  -- Session types
isSessionType _    _    (Skip _)        = True
isSessionType _    _    (Semi _ _ _)    = True
isSessionType _    _    (Message _ _ _) = True
isSessionType _    _    (Choice _ _ _)  = True
  -- Functional or session
isSessionType tenv kenv (Rec _ _ t)     = isSessionType tenv kenv t
isSessionType _    kenv (TypeVar _ x)   = Map.member x kenv
  -- Type operators
isSessionType _    _    (Dualof _ _)    = True
isSessionType tenv kenv (TypeName _ x)  = isSession $ fst $ tenv Map.! x
  -- Otherwise: Functional types
isSessionType _    _    _               = False

-- Type schemes

instance Equivalence TypeScheme where
  equivalent tenv kenv1 ts1 ts2 =
    case instantiate ts1 ts2 of
      Nothing              -> False
      Just (kenv2, t1, t2) -> equivalent tenv (kenv1 `Map.union` kenv2) t1 t2

instantiate :: TypeScheme -> TypeScheme -> Maybe (KindEnv, Type, Type)
instantiate (TypeScheme _ bs1 t1) (TypeScheme _ bs2 t2) = inst bs1 bs2 t1 t2
  where
  inst :: [TypeVarBind] -> [TypeVarBind] -> Type -> Type -> Maybe (KindEnv, Type, Type)
  inst ((TypeVarBind p1 x1 k1):bs1) ((TypeVarBind _ x2 k2):bs2) t1 t2
    | k1 /= k2  = Nothing
    | otherwise = -- substitute x1 for x2 in t2
        fmap (\(m, t1', t2') -> (Map.insert x1 k1 m, t1', t2'))
             (inst bs1 bs2 t1 (Rename.subs (TypeVar p1 x1) x2 t2))
  inst [] [] t1 t2 = Just (Map.empty, t1, t2)
  inst _ _ _ _ = Nothing

-- Typing environments

instance Equivalence VarEnv where
  equivalent tenv kenv env1 env2 =
    Map.size env1 == Map.size env2 &&
    Map.foldlWithKey (\acc b s -> acc &&
                                  b `Map.member` env2 &&
                                  equivalent tenv kenv s (env2 Map.! b)) True env1
