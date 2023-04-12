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

module TACAS2020.TypeToGrammar
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
import qualified Data.Set        as Set
import           Debug.Trace
import           Prelude hiding (Word) -- Word is (re)defined in module Equivalence.Grammar

-- Conversion to context-free grammars

convertToGrammar :: TypeEnv -> [Type] -> Grammar
convertToGrammar tEnv ts = --trace ("subs: " ++ show (subs state))  $
  Grammar (map (subWords (subs state)) xs) (subsAllProds (subs state) (productions state))
  where (xs, state) = runState (mapM typeToGrammar ts) (initial tEnv)

typeToGrammar :: Type -> TransState Word
typeToGrammar t = collect [] (normalise Map.empty t) >> toGrammar (normalise Map.empty t)

toGrammar :: Type -> TransState Word
-- Session types
toGrammar (Skip _) =
  return []
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Message _ p b) = do
  y <- freshVar
  addProductions y $ Map.singleton (MessageLabel p b) []
  return [y]
toGrammar (Labelled _ (Choice v) m) = do
  ms <- tMapM toGrammar m
  y <- freshVar
  addProductions y $ Map.mapKeys (ChoiceLabel v) ms
  return [y]
-- Functional or session (session in this case)
toGrammar (TypeVar _ x) = do      -- x is a polymorphic variable
  y <- freshVar
  addProductions y $ Map.singleton (VarLabel x) []
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
  -- getProd' x (Map.map (++ zs) m)
  addProductions x (Map.map (++ zs) m)
  collect σ' u
collect _ _ = return ()

-- The state of the translation to grammars

data TState = TState {
  productions :: Productions
, nextIndex   :: Int
, typeEnv     :: TypeEnv
, subs        :: Subs
}

type Subs = Map.Map TypeVar TypeVar
type TransState = State TState

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions = Map.empty
, nextIndex   = 1
, typeEnv     = tEnv
, subs        = Map.empty
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

addProduction :: TypeVar -> Label -> Word -> TransState ()
addProduction x l w =
  modify $ \s -> s {productions = insertProduction (productions s) x l w}

addSubs :: (TypeVar, TypeVar) -> TransState ()
addSubs (x,y) =
  modify $ \s -> s {subs= Map.insert x y (subs s)}

getSubs :: TransState Subs
getSubs = do
  s <- get
  return $ subs s

getProd :: Transitions -> TransState TypeVar
getProd ts = do
  ps <- getProductions
  -- case Map.foldrWithKey fold Nothing ps of
  case reverseLookup ts ps of
    Nothing -> do
      y <- freshVar
      addProductions y ts
--      traceM $ "addingProd1: " ++ show y ++ " -> " ++ show ts
      return y
    Just x ->
      return x
  where fold x ts' acc = if prodExists ts' ts then Just x else acc

  -- An attempt to reduce equivalent productions

getProd' :: TypeVar -> Transitions -> TransState TypeVar
getProd' x ts = do
  ps <- getProductions
  reverseLookup' x ts ps >>= \case
    Nothing -> addProductions x ts >> return x
    Just y  -> return y

-- Lookup a key for a value in the map. Probably O(n log n)
reverseLookup :: Eq a => Ord k => a -> Map.Map k a -> Maybe k
reverseLookup a =
  Map.foldrWithKey (\k b acc -> if a == b then Just k else acc) Nothing

-- Lookup a key for a value in the map. Probably O(n log n)
reverseLookup' :: TypeVar -> Transitions -> Productions -> TransState (Maybe TypeVar)
reverseLookup' x a ps =
  Map.foldrWithKey (\k b acc -> compareTrans x k a b >>=
                     \b -> if b then return (Just k) else acc) (return Nothing) ps

prodExists :: Transitions -> Transitions -> Bool
prodExists ts ts' =
  Map.foldrWithKey (\l xs acc -> acc && contains l xs ts) (length ts == Map.size ts') ts'
  where
  contains :: Label -> Word -> Transitions -> Bool
  contains l xs ts = Map.lookup l ts == Just xs

-- TODO: Change these names
-- These are two different concepts
type VisitedProds = Set.Set (TypeVar, TypeVar)
type Goals = Set.Set (TypeVar, TypeVar)
type ToVisitProds = Set.Set (TypeVar, TypeVar)

compareTrans :: TypeVar -> TypeVar -> Transitions -> Transitions -> TransState Bool
compareTrans x y ts1 ts2
  | equalTrans ts1 ts2 = do
      let s = Set.singleton (x,y)
      let res = Map.foldrWithKey (\l w acc -> acc `Set.union`
                                   (compareWords s w (ts2 Map.! l))) Set.empty ts1

      b <- fixedPoint (Set.singleton (x,y)) res x ts1

      if b && not (null res) -- TODO: new fun on where
        then addSubs (x,y) >> return True
        else return False
  | otherwise = return False

-- Verifies if two transitions are equal.
-- That is if the keys on both are the same and if
-- their words have the same size
equalTrans :: Transitions -> Transitions -> Bool
equalTrans ts1 ts2 = Map.keys ts1 == Map.keys ts2 &&
                     (and $ map (\(x,y) -> length x == length y) (zip (Map.elems ts1) (Map.elems ts2)))

fixedPoint :: VisitedProds -> VisitedProds -> TypeVar -> Transitions -> TransState Bool
fixedPoint visited goals w t
  | Set.null goals = return True
  | otherwise      = do
      let (x, y) = Set.elemAt 0 goals
      ts1 <- safeGetTransitions x t
      ps <- getProductions
      if (Map.member y ps)
      then do
        y1 <- applySubs y
        ts2 <- getTransitions y1
        fixedPoint' visited goals ts1 ts2 x y1
      else return False

   where
     -- Recursively calls fixedPoint when the transitions are equal
     fixedPoint' :: VisitedProds -> Goals -> Transitions -> Transitions ->
                    TypeVar -> TypeVar -> TransState Bool
     fixedPoint' visited goals ts1 ts2 x y
       | equalTrans ts1 ts2 = do
          let newG = newGoals ts1 ts2
          fixedPoint (Set.insert (x,y) visited) (updateGoals goals newG x y) w t
       | otherwise = return False

     newGoals :: Transitions -> Transitions -> Goals
     newGoals ts1 ts2 =
       Map.foldrWithKey (\l w acc -> acc `Set.union`
                             compareWords visited (ts1 Map.! l) w) Set.empty ts2

-- Deletes the current goal and updates it with the new ones
updateGoals :: Goals -> Goals -> TypeVar -> TypeVar -> Goals
updateGoals goals newGoals x y = Set.union newGoals (Set.delete (x, y) goals)

applySubs :: TypeVar -> TransState TypeVar
applySubs y = do
  subs <- getSubs
  if Map.member y subs
  then return $ subs Map.! y
  else return y

-- If there are no transitions; returns it's own transitions
safeGetTransitions :: TypeVar -> Transitions -> TransState Transitions
safeGetTransitions x defaultTrans = do
  ps <- getProductions
  case ps Map.!? x of
    Just p -> return p
    Nothing -> return defaultTrans

-- Compares two words
-- If they are on the Set of visited productions, there is no need
-- to visit them. Otherwise, we add them to the set of productions
-- that we still need to explore.
compareWords :: VisitedProds -> Word -> Word -> ToVisitProds
compareWords s xs ys =
      foldl (\acc (x, y) -> if x == y || Set.member (x,y) s
                            then acc
                            else Set.insert (x, y) acc ) Set.empty (zip xs ys)


-- SUBSTITUTION

-- Goes over the productions and substitutes the transitions
subsAllProds :: Subs -> Productions -> Productions
subsAllProds xs = Map.map (subTransition xs)

-- Goes over the transitions and substitutes the words
subTransition :: Subs -> Transitions -> Transitions
subTransition xs = Map.map (subWords xs)

-- Goes over the words to substitute each type var
subWords :: Subs -> Word -> Word
subWords xs = map (subsTypeVars xs)

-- For each type var applies all the given substitutions (named subs)
subsTypeVars :: Subs -> TypeVar -> TypeVar
subsTypeVars subs v = Map.foldrWithKey (\x y w -> if x == w then y else w) v subs
