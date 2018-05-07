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
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

type Productions = Map.Map TypeVar (Map.Map Label Vars)

data GNF = GNF {start :: TypeVar, productions :: Productions} deriving Show

-- The state of the translation to GNF

type Visited = Map.Map Type TypeVar

type GNFState = State (Productions, Visited, Int)

initial :: (Productions, Visited, Int)
initial = (Map.empty, Map.empty, 0)

freshVar :: GNFState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n+1))
  return $ "_x" ++ show n

lookupVisited :: Type -> GNFState (Maybe TypeVar)
lookupVisited t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> GNFState ()
insertVisited t x =
  modify (\(p, v, n) -> (p, Map.insert t x v, n))

getAllProductions :: GNFState Productions
getAllProductions = do
  (p, _, _) <- get
  return p

getProductions :: TypeVar -> GNFState (Map.Map Label Vars)
getProductions x = do
  p <- getAllProductions
  return $ p Map.! x

insertProduction :: TypeVar -> Label -> Vars -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertProductions :: TypeVar -> (Map.Map Label Vars) -> GNFState ()
insertProductions x m = do
  let _ = Map.mapWithKey (\l w -> insertProduction x l w) m
  return ()

replaceInProductions :: Vars -> TypeVar -> GNFState ()
replaceInProductions w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Translation to GNF

-- Assume: t is a session type different from Skip
convertToGNF :: Type -> GNF
convertToGNF t = evalState (generateGNF t) initial

generateGNF :: Type -> GNFState GNF
generateGNF t = do
  (y:v) <- toGNF t
  m <- getProductions y
  insertProductions y (Map.map (\w -> v ++ w) m)
  p <- getAllProductions
  return $ GNF {start = y, productions = p}

toGNF :: Type -> GNFState Vars
toGNF t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGNF' t
    Just x ->  return [x]

toGNF' :: Type -> GNFState Vars
toGNF' Skip = return []
toGNF' (Message p b) = do
  y <- freshVar
  insertProduction y (MessageLabel p b) []
  return [y]
toGNF' (Var x) = do
  y <- freshVar
  insertProduction y (VarLabel x) []
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
  replaceInProductions w x
  return w

assocsToGNF :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> GNFState ()
assocsToGNF _ _ [] = return ()
assocsToGNF y p ((l, t):as) = do
  w <- toGNF t
  insertProduction y (ChoiceLabel p l) w  
  assocsToGNF y p as

-- tests

t1 = convertToGNF $ Message Out IntType
t2 = convertToGNF $ (Var "alpha")
t3 = convertToGNF $ Semi (Message Out IntType) (Message In BoolType)
t4 = convertToGNF $ Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", Message In BoolType)])
linSessionKind = Kind {prekind = Session, multiplicity = Un}
treeSend = Rec (Bind "y" linSessionKind) (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
t5 = convertToGNF treeSend
t6 = convertToGNF $ Semi treeSend (Var "alpha")

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
