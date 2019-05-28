{- |
Module      :  Equivalence.TypeToGrammar
Description :  Conversion from types to grammars
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module builds the initial monadic state, and converts the session types
given as parameter to context-free grammars
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Equivalence.TypeToGrammar
( convertToGrammar
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Validation.Substitution
import           Equivalence.Grammar
import           Equivalence.Normalisation
import           Utils.FreestState (tMapWithKeyM, tMapM)
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Queue
import           Debug.Trace

-- Conversion to context-free grammars

convertToGrammar :: TypeEnv -> [Type] -> Grammar
convertToGrammar tEnv ts = Grammar xs (productions s)
  where (xs, s) = runState (mapM typeToGrammar ts) (initial tEnv)

typeToGrammar :: Type -> TransState TypeVar
typeToGrammar t = do
  xs <- toGrammar t
  eqsToGrammar
  y <- freshVar
  addProduction y (MessageLabel Out UnitType) xs
  return y

toGrammar :: Type -> TransState [TypeVar]
-- Session types
toGrammar (Skip _) =
  return []
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Message _ p b) = do
  getBasicProd (MessageLabel p b)
toGrammar (Choice _ p m) = do
  y <- freshVar
  tMapWithKeyM (fieldToGrammar y p) m
  return [y]
-- Functional or session (session in this case)
toGrammar (TypeVar _ x) = do
  wasVisited x >>= \case
    True  -> return [x]                  -- x is a recursion variable
    False -> getBasicProd (VarLabel x)   -- x is a polymorphic variable
toGrammar t@(Rec _ (TypeVarBind _ x _) u) =
  if x `Set.notMember` (free u)
    then toGrammar u
  else do
    insertEquation t
    insertVisited x
    return [x]
{-
toGrammar (Rec _ (TypeVarBind _ x _) t) =
  if x `Set.notMember` (free t)
  then toGrammar t
  else
  do
  insertVisited x
  m <- typeTransitions t
  transFromX <- tMapWithKeyM (\l _ -> freshVar >>= \y -> addProduction x l [y] >> return y) m
  tMapM insertVisited transFromX
  transFromT <- tMapM toGrammar m
  let subs = Map.foldrWithKey (\l y -> Map.insert y (transFromT Map.! l)) Map.empty transFromX
  subsProductions subs
  return [x]
-}
{-
toGrammar (Rec _ (TypeVarBind _ x _) t) = do
  -- let (Rec _ (TypeVarBind _ x _) u) = rename t -- On the fly α-conversion
  insertVisited x
  toGrammar t >>= \case
    []     -> return []
    (z:zs) ->
      getTransitions z >>= \case
        Just m -> do
          addProductions x (Map.map (++ zs) m)
          return [x]
        Nothing -> do
          b <- wasVisited z
          if b && z /= x then -- case in which the productions for z are not yet completed
            return (z:zs)
          else
            return []
-}
  -- Type operators
toGrammar (Dualof p (TypeName _ x)) = do
  insertVisited x
  (k, TypeScheme _ [] t) <- getFromVEnv x
  trace ("Type " ++ show x ++ " = " ++ show t)
    toGrammar (Dualof p t)
toGrammar (Dualof _ t) = toGrammar (dual t)
toGrammar (TypeName _ x) = do
  b <- wasVisited x
  if b
  then    -- We have visited this type name before
    return [x]
  else do -- This is the first visit
    insertVisited x
    (k, TypeScheme _ [] t) <- getFromVEnv x
    toGrammar t -- (Rec p (TypeVarBind q x k) t)
  -- Should not happen
toGrammar t = error ("toGrammar: " ++ show t)

fieldToGrammar :: TypeVar -> Polarity -> ProgVar -> Type -> TransState ()
fieldToGrammar y p x t = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel p x) xs

eqsToGrammar :: TransState ()
eqsToGrammar = do
  getEquation >>= \case
    (Just (Rec _ (TypeVarBind _ x _) t)) -> do
      (z:zs) <- toGrammar t
      eqsToGrammar
      getTransitions z >>= \case
        Just m -> do
          addProductions x (Map.map (++ zs) m)
        Nothing ->
          return ()
    Nothing ->
      return ()
{-
typeTransitions :: Type -> TransState (Map.Map Label Type)
  -- Session types
typeTransitions (Skip _)        = return Map.empty
typeTransitions (Semi p t u)
  | terminated t                = typeTransitions u
  | otherwise                   = do
  m <- typeTransitions t
  return $ Map.map (\t' -> Semi p t' u) m
typeTransitions (Message p q b) = return $ Map.singleton (MessageLabel q b) (Skip p)
typeTransitions (Choice _ p m)  = return $ Map.mapKeys (ChoiceLabel p) m
  -- Functional or session
typeTransitions (Rec _ (TypeVarBind _ x _) t) = do
  insertVisited x
  typeTransitions t
typeTransitions (TypeVar p x)   =
  getTransitions x >>= \case
    Just m  -> return $ Map.map (\[x] -> TypeVar p x) m
    Nothing -> return $ Map.singleton (VarLabel x) (Skip p)
  -- Type operators
typeTransitions (Dualof _ t)    = typeTransitions t
typeTransitions (TypeName _ _)  = error "Not implemented: typeTransitions TypeName"
-}
-- The state of the translation to grammars

type Visited = Set.Set TypeVar

data TState = TState {
  productions :: Productions
, equations   :: Queue.Seq Type
, visited     :: Visited
, nextIndex   :: Int
, typeEnv     :: TypeEnv
}

type TransState = State TState

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions = Map.empty
, equations   = Queue.empty
, visited     = Set.empty
, nextIndex   = 1
, typeEnv     = tEnv
}

freshVar :: TransState TypeVar
freshVar = do
  s <- get
  let n = nextIndex s
  modify (\s -> s {nextIndex = n + 1})
  return $ mkVar defaultPos ("#X" ++ show n)

wasVisited :: TypeVar -> TransState Bool
wasVisited x = do
  s <- get
  return $ x `Set.member` (visited s)
  -- return $ x `Map.member` (productions s)

insertVisited :: TypeVar -> TransState ()
insertVisited x =
  modify $ \s -> s {visited = Set.insert x (visited s)}

getTransitions :: TypeVar -> TransState (Maybe Transitions)
getTransitions x = do
  s <- get
  return $ (productions s) Map.!? x

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \s -> s {productions = insertProduction (productions s) x l w}

getEquation :: TransState (Maybe Type)
getEquation = do
  s <- get
  case equations s of
    (t Queue.:<| eqs) -> do
      modify $ \s -> s {equations = eqs}
      return $ Just t
    Queue.Empty ->
      return Nothing

insertEquation :: Type -> TransState ()
insertEquation t = do
  modify $ \s -> s {equations = (equations s) Queue.|> t}

{-
subsProductions :: (Map.Map TypeVar [TypeVar]) -> TransState ()
subsProductions m = do
  s <- get
  modify $ \s -> s {productions = (Map.map . Map.map) (subsWord m) (productions s)}

-- σ xs = ys, where σ is a n-substitution [ys1/y1, ..., ysn/yn]
subsWord :: Eq a => Map.Map a [a] -> [a] -> [a]
subsWord m = concat . map (subsVar (Map.assocs m))

-- σ x = ys
subsVar :: Eq a => [(a, [a])] -> a -> [a]
subsVar [] x  = [x]
subsVar ((y,ys):assocs) x
  | y == x    = ys
  | otherwise = subsVar assocs x
-}
-- Add or update production from a (basic) non-terminal; the
-- productions may already contain transitions for the given
-- nonterminal (hence the insertWith and union)
getBasicProd :: Label -> TransState [TypeVar]
getBasicProd l = do
  s <- get
  case Map.foldrWithKey fold Nothing (productions s) of
    Nothing -> do
      y <- freshVar
      addProduction y l []
      return [y]
    Just p ->
      return [p]
  where fold x m acc = if l `Map.member` m && null (m Map.! l) then Just x else acc

getFromVEnv :: TypeVar -> TransState (Kind, TypeScheme)
getFromVEnv x = do
  s <- get
  return $ (typeEnv s) Map.! x
