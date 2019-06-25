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
import qualified Validation.Substitution as Substitution (subsAll) -- no renaming
import           Equivalence.Grammar
import           Equivalence.Normalisation
import           Utils.FreestState (tMapWithKeyM, tMapM, tMapM_)
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Debug.Trace

-- Conversion to context-free grammars

convertToGrammar :: TypeEnv -> [Type] -> Grammar
convertToGrammar tEnv ts = Grammar xs (productions s)
  where (xs, s) = runState (mapM typeToGrammar ts) (initial tEnv)

typeToGrammar :: Type -> TransState TypeVar
typeToGrammar t = do
  xs <- toGrammar t
  collect [] t
  getProd $ Map.singleton (MessageLabel Out UnitType) xs

toGrammar :: Type -> TransState [TypeVar]
-- Session types
toGrammar (Skip _) =
  return []
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Message _ p b) = do
  y <- getProd $ Map.singleton (MessageLabel p b) []
  return [y]
toGrammar (Choice _ p m) = do
  ms <- tMapM toGrammar m
  y <- getProd $ Map.mapKeys (ChoiceLabel p) ms
  return [y]
-- Functional or session (session in this case)
toGrammar (TypeVar _ x) = do
  y <- getProd $ Map.singleton (VarLabel x) []   -- x is a polymorphic variable
  return [y]
toGrammar (Rec _ (TypeVarBind _ x _) _) =
  return [x]
  -- Type operators
-- toGrammar (Dualof p (TypeName _ x)) = do
--   insertVisited x
--   (k, TypeScheme _ [] t) <- getFromVEnv x
--   trace ("Type " ++ show x ++ " = " ++ show t)
--     toGrammar (Dualof p t)
-- toGrammar (Dualof _ t) = toGrammar (dual t)
-- toGrammar (TypeName _ x) = do
--   b <- wasVisited x
--   if b
--   then    -- We have visited this type name before
--     return [x]
--   else do -- This is the first visit
--     insertVisited x
--     (k, TypeScheme _ [] t) <- getFromVEnv x
--     toGrammar t
  -- Should not happen
toGrammar t = error ("toGrammar: " ++ show t)

type Substitution = (Type, TypeVar)

collect :: [Substitution] -> Type -> TransState ()
collect σ (Semi _ t u) = collect σ t >> collect σ u
collect σ (Choice _ _ m) = tMapM_ (collect σ) m
collect σ t@(Rec _ (TypeVarBind _ x _) u) = do
  let σ' = (t, x) : σ
  let u' = Substitution.subsAll σ' u
  (z:zs) <- toGrammar (normalise Map.empty u')
  m <- getTransitions z
  addProductions x (Map.map (++ zs) m)
  collect σ' u
collect _ _ = return ()

-- The state of the translation to grammars

data TState = TState {
  productions :: Productions
, nextIndex   :: Int
, typeEnv     :: TypeEnv
}

type TransState = State TState

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions = Map.empty
, nextIndex   = 1
, typeEnv     = tEnv
}

freshVar :: TransState TypeVar
freshVar = do
  s <- get
  let n = nextIndex s
  modify (\s -> s {nextIndex = n + 1})
  return $ mkVar defaultPos ("#X" ++ show n)

getProductions :: TransState Productions
getProductions = do
  s <- get
  return $ productions s
  
getTransitions :: TypeVar -> TransState Transitions
getTransitions x = do
  ps <- getProductions
  return $ ps Map.! x

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \s -> s {productions = insertProduction (productions s) x l w}

getProd :: Transitions -> TransState TypeVar
getProd ts = do
  ps <- getProductions
  -- case Map.foldrWithKey fold Nothing ps of
  case reverseLookup ts ps of
    Nothing -> do
      y <- freshVar
      addProductions y ts
      return y
    Just x ->
      return x
  where fold x ts' acc = if prodExists ts' ts then Just x else acc

-- Lookup a key for a value in the map. Probably O(n log n)
reverseLookup :: Eq a => Ord k => a -> Map.Map k a -> Maybe k
reverseLookup a = 
  Map.foldrWithKey (\k b acc -> if a == b then Just k else acc) Nothing

prodExists :: Transitions -> Transitions -> Bool
prodExists ts ts' =
  Map.foldrWithKey (\l xs acc -> acc && contains l xs ts) (length ts == Map.size ts') ts'
  where
  contains :: Label -> [TypeVar] -> Transitions -> Bool
  contains l xs ts = Map.lookup l ts == Just xs
