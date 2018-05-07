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

--type Node = Set.Set (Vars, Vars)

data Label =
  ChoiceLabel Polarity Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

instance Label Show where
  show (ChoiceLabel In l) = '&' : l
  show (ChoiceLabel Out l) = '+' : l
  show (MessageLabel In t) = '?' : show t
  show (MessageLabel Out t) = '!' : show t
  show (VarLabel l) = l

type Productions = Map.Map (TypeVar, Label) Vars

data GNF = GNF {productions :: Productions, start :: TypeVar} deriving Show

-- Managing the state of translation to GNF procedure

type Visited = Map.Map Type TypeVar
type GNFState = State (Productions, Visited, Int)

initial :: (Productions, Visited, Int)
initial = (Map.empty, Map.empty, 0)

freshVar :: GNFState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n+1))
  return $ "_x" ++ show n

visitedLookup :: Type -> GNFState (Maybe TypeVar)
visitedLookup t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> GNFState ()
insertVisited t x = modify (\(p, v, n) -> (p, Map.insert t x v, n))

insertProduction :: TypeVar -> Label -> Vars -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insert (x, l) w p, v, n))

backPatchProductions :: Vars -> TypeVar -> GNFState ()
backPatchProductions w x =
  modify (\(p, v, n) -> (Map.map (replace w x) p, v, n))

replace :: Vars -> TypeVar -> Vars -> Vars
replace _ _ [] = []
replace w y (x:xs) 
  | x == y    = w ++ (replace w y xs)
  | otherwise = x : (replace w y xs)

-- Translation to GNF

-- Assume: t is a session type
convertToGNF :: Type -> GNF
convertToGNF t =
  case runState (toGNF t) initial of
    ([x], (p, _, _)) -> GNF {productions = p, start = x}
--    (w, (p, _, _)) -> TODO

toGNF :: Type -> GNFState Vars
toGNF t = do
  maybe <- visitedLookup t
  case maybe of
    Nothing -> toGNF' t
    Just x ->  return [x]

toGNF' :: Type -> GNFState Vars
toGNF' Skip = return []
toGNF' (Message p b) = do
  y <- freshVar
  insertProduction y (MessageLabel p b) []
  return [y]
--toGNF' (Semi (Choice p m) t) = do --- TODO
toGNF' (Semi t1 t2) = do
  w1 <- toGNF t1
  w2 <- toGNF t2
  return $ w1 ++ w2
toGNF' (Choice p m) = do
  y <- freshVar
  assocsToGNF y p (Map.assocs m)
  return [y]
toGNF' (Rec (Bind x k) t) = do
  insertVisited (Rec (Bind x k) t) x
  w <- toGNF (unfold (Rec (Bind x k) t))
  backPatchProductions w x
  return w
--toGNF' x where x is a free type variable

assocsToGNF :: TypeVar -> Polarity -> [(Constructor, Type)] -> GNFState ()
assocsToGNF _ _ [] = return ()
assocsToGNF y p ((l, t):as) = do
  w <- toGNF t
  insertProduction y (ChoiceLabel p l) w  
  assocsToGNF y p as

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
