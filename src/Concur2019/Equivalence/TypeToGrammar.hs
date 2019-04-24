
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

import           Equivalence.Grammar
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace

-- Conversion to context-free grammars

convertToGrammar :: TypeEnv -> [Type] -> Grammar
convertToGrammar tEnv ts = Grammar xs (productions s)
  where (xs, s) = runState (mapM typeToGrammar ts) (initial tEnv)

typeToGrammar :: Type -> TransState TypeVar
typeToGrammar t = do
  xs <- toGrammar t
  y <- freshVar
  addProduction y (MessageLabel In UnitType) xs
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
  y <- addBasicProd (MessageLabel p b)
  return [y]
toGrammar (Choice _ p m) = do
  y <- freshVar
  mapM_ (assocToGrammar y p) (Map.assocs m)
  return [y]
-- Functional or session (session in this case)
toGrammar (TypeVar _ x) = do
  b <- memberVisited x
  if b
  then    -- This is a recursion variable
    return [x]
  else do -- This is a polymorphic variable
    y <- addBasicProd (VarLabel x)
    return [y]
toGrammar (Rec _ (TypeVarBind _ x _) t) = do
  insertVisited x
  toGrammar t >>= \case
    []     -> return []
    (z:zs) ->
      getTransitions z >>= \case
        Just m -> do
          addProductions x (Map.map (++ zs) m)
          return [x]
        Nothing ->
          return []
toGrammar (Dualof _ t) = toGrammar (dual t)
toGrammar (TypeName p x) = do
  b <- memberVisited x
  if b
  then    -- We have visited this type name before
    return [x]
  else do -- This is the first visit
    (k, TypeScheme q [] t) <- getFromVEnv x
    trace ("TypeName: " ++ show (Rec p (TypeVarBind q x k) t))
      toGrammar (Rec p (TypeVarBind q x k) t)
  -- Should not happen
toGrammar t = trace ("toGrammar: " ++ show t) (return [mkVar (position t) "error"])

assocToGrammar :: TypeVar -> Polarity -> (ProgVar, Type) -> TransState ()
assocToGrammar y p (x, t) = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel p x) xs

-- The state of the translation to grammars

type Visited = Set.Set TypeVar

data TState = TState {
  productions :: Productions
, visited     :: Visited
, nextIndex   :: Int
, typeEnv     :: TypeEnv  
}

type TransState = State TState

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions = Map.empty
, visited     = Set.empty
, nextIndex   = 1
, typeEnv     = tEnv
}

freshVar :: TransState TypeVar
freshVar = do
  s <- get
  let n = nextIndex s
  modify (\s -> s{nextIndex = n + 1})
  return $ mkVar defaultPos ("#X" ++ show n)

memberVisited :: TypeVar -> TransState Bool
memberVisited t = do
  s <- get
  return $ Set.member t (visited s)

insertVisited :: TypeVar -> TransState ()
insertVisited x =
  modify $ \s -> s{visited=Set.insert x (visited s)}

getTransitions :: TypeVar -> TransState (Maybe Transitions)
getTransitions x = do
  s <- get
  return $ (productions s) Map.!? x

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \s -> s{productions=insertProduction (productions s) x l w}

-- Add or update production from a (basic) non-terminal; the
-- productions may already contain transitions for the given
-- nonterminal (hence the insertWith and union)
addBasicProd :: Label -> TransState TypeVar
addBasicProd l = do
  s <- get
  let p' = Map.filter (Map.member l) (productions s)
  if Map.null p'
    then do
      y <- freshVar
      addProduction y l []
      return y
    else do
      let (y,_) = Map.elemAt 0 p'
      return y

getFromVEnv :: TypeVar -> TransState (Kind, TypeScheme)
getFromVEnv x = do
  s <- get
  return $ (typeEnv s) Map.! x
