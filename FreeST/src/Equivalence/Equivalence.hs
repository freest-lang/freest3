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
  )
where

import           Bisimulation.Bisimulation ( bisimilar )
import           Syntax.Base
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error ( internalError )
import           Util.FreestState              ( initialState
                                                , errors
                                                )
import           Validation.Kinding ( synthetise )
import           Validation.Subkind ( (<:) )
import qualified Validation.Substitution as Subs
                                                ( unfold
                                                , subs
                                                )

import           Control.Monad.State ( runState )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


class Equivalence t where
  equivalent :: K.KindEnv -> t -> t -> Bool

type Visited = Set.Set (Span, Span)

-- A co-inductive definition for functional types. A bisimulation
-- based definition for session types
instance Equivalence T.Type where
  -- equivalent _ = bisimilar
  equivalent = equiv Set.empty
   where
    equiv :: Visited -> K.KindEnv -> T.Type -> T.Type -> Bool
    -- Have we been here before?
    equiv v _ t1 t2 | (getSpan t1, getSpan t2) `Set.member` v  = True
    -- Almanac
    equiv v kEnv (T.Almanac _ T.Variant m1) (T.Almanac _ T.Variant m2) =
      Map.size m1
        == Map.size m2
        && Map.foldlWithKey (equivField v kEnv m2) True m1
    -- Session types
    equiv _ kEnv t1 t2 | isSessionType kEnv t1 && isSessionType kEnv t2 =
      bisimilar t1 t2
    -- Functional types
    equiv _ _ (T.Int  _) (T.Int  _)                    = True
    equiv _ _ (T.Char _) (T.Char _)                    = True
    equiv _ _ (T.Bool _) (T.Bool _)                    = True
    equiv _ _ (T.Unit _) (T.Unit _)                    = True
    equiv _ _ (T.String _) (T.String _)                = True
    equiv v kEnv (T.Arrow _ n1 t1 t2) (T.Arrow _ n2 u1 u2) =
      n1 == n2 && equiv v kEnv t1 u1 && equiv v kEnv t2 u2
    equiv v kEnv (T.Pair _ t1 t2) (T.Pair _ u1 u2) =
      equiv v kEnv t1 u1 && equiv v kEnv t2 u2
    -- Polymorphism and recursion
    equiv v kEnv (T.Forall _ (Bind p a1 k1 t1)) (T.Forall _ (Bind _ a2 k2 t2))
      = k1 <: k2 && k2 <: k1 &&
           equiv v (Map.insert a1 k1 kEnv) t1
            (Subs.subs (T.Var p a1) a2 t2)
    equiv v kEnv t1@T.Rec{} t2 =
      equiv (Set.insert (getSpan t1, getSpan t2) v) kEnv (Subs.unfold t1) t2
    equiv v kEnv t1 t2@T.Rec{} =
      equiv (Set.insert (getSpan t1, getSpan t2) v) kEnv t1 (Subs.unfold t2)
    equiv _ _ (T.Var _ a1) (T.Var _ a2) = a1 == a2 -- Polymorphic variable
    -- Should not happen
    equiv _ _ t1@T.Dualof{} _ =
      internalError "Equivalence.Equivalence.equivalent" t1
    equiv _ _ _ t2@T.Dualof{} =
      internalError "Equivalence.Equivalence.equivalent" t2
    equiv _ _ _ _ = False

    equivField
      :: Visited -> K.KindEnv -> T.TypeMap -> Bool -> Variable -> T.Type -> Bool
    equivField v kEnv m acc l t =
      acc && l `Map.member` m && equiv v kEnv (m Map.! l) t

isSessionType :: K.KindEnv -> T.Type -> Bool
isSessionType kEnv t = null (errors state) && K.isSession kind
 where
  (kind, state) = runState (synthetise kEnv t) initialState

{-
-- An alternative is below. Lighter, but I don't have a proof that the
-- predicates are equivalent.

isSessionType :: K.KindEnv -> T.Type -> Bool
isSessionType _ T.Skip{} = True
isSessionType _ T.End{}  = True
isSessionType _ T.Semi{} = True
isSessionType _ T.Message{} = True
isSessionType _ T.Almanac _ (T.Choice v) _ = True
isSessionType _ (T.Rec _ (Bind _ _ k) _) = K.isSession k
isSessionType kEnv (T.Var _ x) = Map.member x kEnv -- Polymorphic variable
isSessionType _ t@T.Dualof{} = internalError "Equivalence.Equivalence.isSessionType" t
isSessionType _ _  = False
-}

instance Equivalence VarEnv where
  equivalent kenv env1 env2 =
    Map.size env1
      == Map.size env2
      && Map.foldlWithKey
           (\acc b s ->
             acc && b `Map.member` env2 && equivalent kenv s (env2 Map.! b)
           )
           True
           env1
