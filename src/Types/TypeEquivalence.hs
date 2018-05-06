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

module Types.TypeEquivalence(
  equivalent
, unfold
, subs  
) where

import Types.Types
import Types.Kinds
import Types.Kinding
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- GREIBACH NORMAL FORMS

type Vars = [TypeVar]

type Node = Set.Set (Vars, Vars)

data Label =
  ChoiceLabel ChoiceView TypeVar |
  OutLabel BasicType |
  InLabel BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord, Show)

data GNF = GNF {productions :: Map.Map (TypeVar, Label) Vars, start :: TypeVar}

-- Assume: t is a session type
convertToGNF :: Type -> GNF
convertToGNF t = toGBF (GNF Map.empty  t

toGBF :: Type 
toGBF Skip = []
toGBF (Var x) = [NonTerm x]
toGBF (Out b) = [TermOut b]
toGBF (In b) = [TermIn b]
toGBF (Var a) = [TermVar a]
toGBF (Choice v m) = [TermVar a]




-- TYPE EQUIVALENCE

equivalent :: KindEnv -> Type -> Type -> Bool
equivalent _ _ _ = True

{-
bisim :: Word -> Word -> GNF -> Bool
bisim w1 w2 = check [simplify (Set.singleton (w1, w2))]

check :: [Node] -> GNF -> Bool
check ns gnf =
  Set.isempty ns || expandNode (head ns)

-}



-- Assumes parameter is a Rec type
unfold :: Type -> Type
unfold (Rec (Bind x k) t) = subs (Rec (Bind x k) t) x t

subs :: Type -> TypeVar -> Type -> Type
subs t y (Var x)
    | x == y                = t
    | otherwise             = Var x
subs t y (Semi t1 t2)       = Semi (subs t y t1) (subs t y t2)
subs t y (PairType t1 t2)   = PairType (subs t y t1) (subs t y t2)
subs t2 y (Forall x k t1)
    | x == y                = Forall x k t1
    | otherwise             = Forall x k (subs t2 y t1)
-- Assume y /= x 
subs t2 y (Rec (Bind x k) t1)
    | x == y                = Rec (Bind x k) t1
    | otherwise             = Rec (Bind x k) (subs t2 y t1)
subs t y (Choice v m)       = Choice v (Map.map(subs t y) m)
subs t y (Fun m t1 t2)      = Fun m (subs t y t1) (subs t y t2)
subs _ _ t                  = t
-- subs _ _ Skip               = Skip
-- subs _ _ (In b)             = In b
-- subs _ _ (Out b)            = Out b
