{- |
Module      :  Equivalence.AlphaCongruence
Description :  Alpha equivalence for types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Type equality up to bound variable renaming
-}

module Equivalence.AlphaCongruence
  (
  )
where

import           Syntax.Base
import qualified Syntax.Type                   as T
import qualified Data.Map.Strict               as Map

type VarMap = Map.Map Variable Variable

class Equiv t where
  equiv :: VarMap -> t -> t -> Bool

instance Eq T.Type where
  t == u = equiv Map.empty t u

instance Equiv T.Type where
  equiv _ T.Int{} T.Int{} = True
  equiv _ T.Float{} T.Float{} = True
  equiv _ T.Char{} T.Char{} = True
  equiv _ T.String{} T.String{} = True
  equiv v (T.Arrow _ m1 t1 u1) (T.Arrow _ m2 t2 u2) =
    m1 == m2 && equiv v t1 t2 && equiv v u1 u2
  equiv v (T.Labelled _ s1 m1) (T.Labelled _ s2 m2) =
    s1 == s2 &&
    Map.size m1 == Map.size m2 &&
    Map.isSubmapOfBy (equiv v) m1 m2
  equiv _ T.Skip{} T.Skip{} = True
  equiv _ (T.End _ p1) (T.End _ p2) = p1 == p2
  equiv v (T.Semi _ t1 u1) (T.Semi _ t2 u2) = equiv v t1 t2 && equiv v u1 u2
  equiv v (T.Message _ l1 p1 t1) (T.Message _ l2 p2 t2) = p1 == p2 && l1 == l2 && equiv v t1 t2
  equiv v (T.Forall _ b1) (T.Forall _ b2) = equiv v b1 b2
  equiv v (T.Rec _ b1) (T.Rec _ b2) = equiv v b1 b2
  equiv v (T.Var _ x1) (T.Var _ x2) =
    x1 == x2 ||                -- free variables
    Just x2 == Map.lookup x1 v -- bound variables
  equiv v (T.Dualof _ t1) (T.Dualof _ t2) = equiv v t1 t2
  equiv _ _ _ = False

instance (Equiv t, Eq k) => Equiv (Bind k t) where
  equiv v (Bind _ x1 _ t1) (Bind _ x2 _ t2) = equiv (Map.insert x1 x2 v) t1 t2
