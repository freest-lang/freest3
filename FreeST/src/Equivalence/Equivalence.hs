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

-- import           Syntax.Schemes
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import           Syntax.TypeVariable
-- import qualified Validation.Rename             as Rename
--                                                 ( subs
--                                                 , unfold
--                                                 )
import qualified Validation.Substitution       as Subs
                                                ( -- subs,
                                                  unfold
                                                )
-- import           Bisimulation.Grammar
import           Bisimulation.Bisimulation     as Bisimulation
import           Equivalence.TypeToGrammar
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

class Equivalence t where
  equivalent :: T.TypeEnv -> K.KindEnv -> t -> t -> Bool

-- Types

type Visited = Set.Set (T.Type, T.Type)

-- A co-inductive definition for functional types. A bisimulation
-- based definition for session types
instance Equivalence T.Type where
  equivalent tenv kenv = equiv Set.empty
   where
    equiv :: Visited -> T.Type -> T.Type -> Bool
    -- Have we been here before?
    equiv v t u | (t, u) `Set.member` v   = True
    -- Functional types
    equiv _ (T.IntType  _) (T.IntType  _) = True
    equiv _ (T.CharType _) (T.CharType _) = True
    equiv _ (T.BoolType _) (T.BoolType _) = True
    equiv _ (T.UnitType _) (T.UnitType _) = True
    equiv v (T.Fun _ m t1 t2) (T.Fun _ n u1 u2) =
      m == n && equiv v t1 u1 && equiv v t2 u2
    equiv v (T.Pair _ t1 t2) (T.Pair _ u1 u2) = equiv v t1 u1 && equiv v t2 u2
    equiv v (T.Datatype _ m1) (T.Datatype _ m2) =
      Map.size m1 == Map.size m2 && Map.foldlWithKey (equivField v m2) True m1
    -- Polymorphism
    equiv v (T.Forall _ kb1 t) (T.Forall _ kb2 u) = -- TODO: check kindbinds ?
      -- kb1 == kb2 &&
      equiv (Set.insert (t, u) v) t u
    -- Recursion
    equiv _ (T.TypeVar _ x) (T.TypeVar _ y) = x == y -- A free (a polymorphic) type var
    equiv v t@T.Rec{} u = equiv (Set.insert (t, u) v) (Subs.unfold t) u
    equiv v t u@T.Rec{} = equiv (Set.insert (t, u) v) t (Subs.unfold u)
    -- Type operators
    equiv _ (T.TypeName _ x) (T.TypeName _ y) = -- trace ("TNAME 1 " ++ show x) $
      x == y -- Admissible
    equiv v (T.TypeName _ x) u = -- trace ("TNAME 2 " ++ show x) $
      equiv v (getType x) u
    equiv v t (T.TypeName _ y) = -- trace ("TNAME 3 " ++ show y) $
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
isSessionType _    kenv (T.TypeVar _ x           ) = Map.member x kenv
  -- Type operators
isSessionType _    _    T.Dualof{}                 = True
isSessionType tenv _ (T.TypeName _ x) = K.isSession $ fst $ tenv Map.! x
  -- Otherwise: Functional types
isSessionType _    _    _                          = False

-- Type schemes

-- instance Equivalence TypeScheme where
--   equivalent tenv kenv1 ts1 ts2 = case instantiate ts1 ts2 of
--     Nothing              -> False
--     Just (kenv2, t1, t2) -> equivalent tenv (kenv1 `Map.union` kenv2) t1 t2

-- instantiate :: TypeScheme -> TypeScheme -> Maybe (K.KindEnv, Type, Type)
-- instantiate (TypeScheme _ bs1 t1) (TypeScheme _ bs2 t2) = inst bs1 bs2 t1 t2
--  where
--   inst
--     :: [K.Bind]
--     -> [K.Bind]
--     -> Type
--     -> Type
--     -> Maybe (K.KindEnv, Type, Type)
--   inst (K.Bind p1 x1 k1 : bs1) (K.Bind _ x2 k2 : bs2) t1 t2
--     | k1 /= k2 = Nothing
--     | otherwise = -- substitute x1 for x2 in t2
--                   fmap (\(m, t1', t2') -> (Map.insert x1 k1 m, t1', t2'))
--                        (inst bs1 bs2 t1 (Rename.subs (TypeVar p1 x1) x2 t2))
--   inst [] [] t1 t2 = Just (Map.empty, t1, t2)
--   inst _  _  _  _  = Nothing

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
