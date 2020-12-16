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

import           Syntax.Base                    ( Pos, pos )
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Bisimulation.Bisimulation      ( bisimilar )
import qualified Validation.Substitution       as Subs
                                                ( unfold, subs )
import           Validation.Subkind             ( (<:) )
import           Utils.Error                    ( internalError )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

class Equivalence t where
  equivalent :: K.KindEnv -> t -> t -> Bool

type Visited = Set.Set (Pos, Pos)

-- A co-inductive definition for functional types. A bisimulation
-- based definition for session types
instance Equivalence T.Type where
  equivalent = equiv Set.empty
   where
    equiv :: Visited -> K.KindEnv -> T.Type -> T.Type -> Bool
    -- Have we been here before?
    equiv v _ t1 t2 | (pos t1, pos t2) `Set.member` v = True
    -- Functional types
    equiv _ _ (T.Int _) (T.Int _) = True
    equiv _ _ (T.Char _) (T.Char _) = True
    equiv _ _ (T.Bool _) (T.Bool _) = True
    equiv _ _ (T.Unit _) (T.Unit _) = True
    equiv v kEnv (T.Fun _ n1 t1 t2) (T.Fun _ n2 u1 u2) =
      n1 == n2 && equiv v kEnv t1 u1 && equiv v kEnv t2 u2
    equiv v kEnv (T.Pair _ t1 t2) (T.Pair _ u1 u2) =
      equiv v kEnv t1 u1 && equiv v kEnv t2 u2
    equiv v kEnv (T.Datatype _ m1) (T.Datatype _ m2) =
      Map.size m1 == Map.size m2 && Map.foldlWithKey (equivField v kEnv m2) True m1
    -- Polymorphism and recursion
    equiv v kEnv (T.Forall _ (K.Bind p a1 k1) t1) (T.Forall _ (K.Bind _ a2 k2) t2) =
      k1 <: k2 && k2 <: k1 &&
      equiv v (Map.insert a1 k1 kEnv) t1 (Subs.subs (T.Var p a1) a2 t2)
    equiv v kEnv t1@T.Rec{} t2 =
      equiv (Set.insert (pos t1, pos t2) v) kEnv (Subs.unfold t1) t2
    equiv v kEnv t1 t2@T.Rec{} =
      equiv (Set.insert (pos t1, pos t2) v) kEnv t1 (Subs.unfold t2)
    equiv _ _ (T.Var _ a1) (T.Var _ a2) = a1 == a2 -- Polymorphic variable
    -- Session types
    equiv _ kEnv t1 t2 | isSessionType kEnv t1 && isSessionType kEnv t2 =
      bisimilar t1 t2
    -- Should not happen
    equiv _ _ t1@T.Dualof{} _ = internalError "Equivalence.Equivalence.equivalent" t1
    equiv _ _ _ t2@T.Dualof{} = internalError "Equivalence.Equivalence.equivalent" t2
    equiv _ _ _ _ = False

    equivField :: Visited -> K.KindEnv -> T.TypeMap -> Bool -> ProgVar -> T.Type -> Bool
    equivField v kEnv m acc l t = acc && l `Map.member` m && equiv v kEnv (m Map.! l) t

-- An alternative is to call Validation.Kinding.synthetise and check whether the
-- synthetised kind is a session type. This would be as in the paper but a lot
-- heavier. I don't kind we have a prove that the predicates are equivalent.
isSessionType :: K.KindEnv -> T.Type -> Bool
isSessionType _ T.Skip{} = True
isSessionType _ T.Semi{} = True
isSessionType _ T.Message{} = True
isSessionType _ T.Choice{} = True
isSessionType _ (T.Rec _ (K.Bind _ _ k) _) = K.isSession k
isSessionType kEnv (T.Var _ x) = Map.member x kEnv -- Polymorphic variable
isSessionType _ t@T.Dualof{} = internalError "Equivalence.Equivalence.isSessionType" t
isSessionType _ _  = False

instance Equivalence T.VarEnv where
  equivalent kenv env1 env2 =
    Map.size env1 == Map.size env2
      && Map.foldlWithKey
           (\acc b s -> acc && b `Map.member` env2 && equivalent
             kenv
             s
             (env2 Map.! b)
           )
           True
           env1
