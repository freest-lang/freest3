{- |
Module      :  Bisimulation.AlphaEquivalence
Description :  Alpha equivalence for types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Trinary type equality up to bound variable renaming. Possible outputs: True |
False | Unknown as per data Trinary.
-}

module Bisimulation.AlphaEquivalenceTrinary
  ( trinary
  )
where

import           Syntax.Base
import qualified Syntax.Type                   as T
import qualified Data.Map.Strict               as Map
import           Bisimulation.ThreeValuedLogic as TVL

trinary :: T.Type -> T.Type -> TVL.Trinary
trinary = equiv Map.empty

instance Eq T.Type where
  t == u = toBool $ trinary t u

type VarMap = Map.Map Variable Variable

class Equiv t where
  equiv :: VarMap -> t -> t -> TVL.Trinary

instance Equiv T.Type where
  equiv _ T.Int{} T.Int{} = TVL.True
  equiv _ T.Int{} _       = TVL.False
  equiv _ _       T.Int{} = TVL.False
  equiv _ T.Float{} T.Float{} = TVL.True
  equiv _ T.Float{} _         = TVL.False
  equiv _ _         T.Float{} = TVL.False
  equiv _ T.Char{} T.Char{} = TVL.True
  equiv _ T.Char{} _        = TVL.False
  equiv _ _        T.Char{} = TVL.False
  equiv _ T.String{} T.String{} = TVL.True
  equiv _ T.String{} _          = TVL.False
  equiv _ _          T.String{} = TVL.False
  equiv v (T.Arrow _ m1 t1 u1) (T.Arrow _ m2 t2 u2) = fromBool(m1 == m2) &&& equiv v t1 t2 &&& equiv v u1 u2
  equiv _ T.Arrow{} _         = TVL.False
  equiv _ _         T.Arrow{} = TVL.False
  equiv v (T.Labelled _ s1 m1) (T.Labelled _ s2 m2) =
    fromBool (s1 == s2 &&
    Map.size m1 == Map.size m2 &&
    Map.isSubmapOfBy (\t1 -> \t2 -> toBool $ equiv v t1 t2) m1 m2)
  equiv _ T.Labelled{} _            = TVL.False
  equiv _ _            T.Labelled{} = TVL.False
  equiv _ T.Skip{} T.Skip{} = TVL.True
  equiv _ T.Skip{} _        = TVL.False
  equiv _ _        T.Skip{} = TVL.False
  equiv _ T.End{} T.End{} = TVL.True
  equiv _ T.End{} _       = TVL.False
  equiv _ _       T.End{} = TVL.False
  equiv v (T.Semi _ t1 u1) (T.Semi _ t2 u2) = equiv v t1 t2 &&& equiv v u1 u2
  equiv v (T.Message _ p1 t1) (T.Message _ p2 t2) = fromBool (p1 == p2) &&& equiv v t1 t2
  equiv _ T.Message{} _           = TVL.False
  equiv _ _           T.Message{} = TVL.False
  equiv v (T.Forall _ b1) (T.Forall _ b2) = equiv v b1 b2
  equiv _ T.Forall{} _          = TVL.False
  equiv _ _          T.Forall{} = TVL.False
  equiv v (T.Rec _ b1) (T.Rec _ b2) = equiv v b1 b2
  equiv v (T.Var _ x1) (T.Var _ x2) = fromBool $ Just x2 == Map.lookup x1 v
  equiv _ T.Var{} _       = TVL.False
  equiv _ _       T.Var{} = TVL.False
  equiv v (T.Dualof _ t1) (T.Dualof _ t2) = equiv v t1 t2
  equiv _ T.Dualof{} _          = TVL.False
  equiv _ _          T.Dualof{} = TVL.False
  equiv _ _ _ = TVL.Unknown

instance Equiv t => Equiv (Bind k t) where
  equiv v (Bind _ x1 _ t1) (Bind _ x2 _ t2) = equiv (Map.insert x1 x2 v) t1 t2
