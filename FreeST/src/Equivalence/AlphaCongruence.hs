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
import           Debug.Trace (trace)
import Parse.Unparser

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
  equiv v (T.Arrow _ m1 l1 l2 t1 u1) (T.Arrow _ m2 l3 l4 t2 u2) =
    m1 == m2 && equiv v l1 l3 && equiv v l2 l4 && equiv v t1 t2 && equiv v u1 u2
    -- m1 == m2 && l1 == l3 && l2 == l4 && equiv v t1 t2 && equiv v u1 u2
  equiv v (T.Labelled _ s1 l1 m1) (T.Labelled _ s2 l2 m2) =
    s1 == s2 &&
    equiv v l1 l2 &&
    -- l1 == l2 &&
    Map.size m1 == Map.size m2 &&
    Map.isSubmapOfBy (equiv v) m1 m2
  equiv _ T.Skip{} T.Skip{} = True
  equiv v (T.End _ p1 l1) (T.End _ p2 l2) = p1 == p2 && equiv v l1 l2
  -- equiv v (T.End _ p1 l1) (T.End _ p2 l2) = p1 == p2 && l1 == l2
  equiv v (T.Semi _ (T.Skip _) t) u = equiv v t u
  equiv v t (T.Semi _ (T.Skip _) u) = equiv v t u
  equiv v (T.Semi _ t1 u1) (T.Semi _ t2 u2) = equiv v t1 t2 && equiv v u1 u2
  equiv v (T.Message _ l1 p1 t1) (T.Message _ l2 p2 t2) = p1 == p2 && equiv v l1 l2 && equiv v t1 t2
  -- equiv v (T.Message _ l1 p1 t1) (T.Message _ l2 p2 t2) = p1 == p2 && l1 == l2 && equiv v t1 t2
  equiv v (T.Forall _ b1) (T.Forall _ b2) = equiv v b1 b2
  -- equiv v (T.Rec _ b1) (T.Rec _ b2) = equiv v b1 b2
  equiv v (T.Rec p1 b1@(Bind _ _ _ t1)) (T.Rec p2 b2@(Bind _ _ _ t2)) = equiv v b1 b2
  equiv v (T.Var _ x1) (T.Var _ x2) =
    x1 == x2 ||                -- free variables
    Just x2 == Map.lookup x1 v -- bound variables
  equiv v (T.Dualof _ t1) (T.Dualof _ t2) = equiv v t1 t2
  equiv v (T.PForall _ b1) (T.PForall _ b2) = equiv v b1 b2
  equiv _ _ _ = False

instance (Equiv t, Eq k) => Equiv (Bind k t) where
  equiv v (Bind _ x1 _ t1) (Bind _ x2 _ t2) = equiv (Map.insert x1 x2 v) t1 t2

instance Equiv T.Level where
  equiv _ T.Top T.Top = True
  equiv _ T.Bottom T.Bottom = True
  equiv v (T.LVar x1) (T.LVar x2) = extern x1 == extern x2
  equiv v (T.LNum n1) (T.LNum n2) = n1 == n2
  equiv v (T.LAdd l1 l2) (T.LAdd l3 l4) = equiv v l1 l3 && equiv v l2 l4
  equiv v (T.LParens l1) (T.LParens l2) = equiv v l1 l2
  equiv _ _ _ = False
