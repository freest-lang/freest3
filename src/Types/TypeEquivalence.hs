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
import Control.Monad.State

-- GREIBACH NORMAL FORMS

type Vars = [TypeVar]

type Node = Set.Set (Vars, Vars)

data Label =
  ChoiceLabel Polarity TypeVar |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

type Productions = Map.Map (TypeVar, Label) Vars

data GNF = GNF {productions :: Productions, start :: TypeVar}

-- State for the translation to GNF procedure

type Visited = Map.Map Type TypeVar
type GNFState = State (Visited, Int)

freshVar :: GNFState String
freshVar = do
  (_,n) <- get
  modify (\(v,n) -> (v,n+1))
  return $ "_x" ++ show n

wasVisited :: Type -> GNFState Bool
wasVisited t = do
  (v,_) <- get
  return $ Map.member t v

visit :: Type -> TypeVar -> GNFState ()
visit t x = modify (\(v,n) -> (Map.insert t x v,n))

initial :: (Visited, Int)
initial = (Map.empty, 0)

-- Translation to GNF

-- Assume: t is a session type
convertToGNF :: Type -> GNF
convertToGNF t =
  case runState (toGBF t) initial of
    (([x], ps), _) -> GNF {productions = ps, start = x}

toGBF :: Type -> GNFState (Vars, Productions)
toGBF Skip = return ([], Map.empty)
-- toGBF (Var x) = [NonTerm x]
toGBF (Message p b) = do
  x <- freshVar
  return ([x], Map.singleton (x, MessageLabel p b) [])
-- toGBF (Var a) = [TermVar a]
-- toGBF (Choice v m) = [TermVar a]



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
