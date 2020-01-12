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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring, TypeSynonymInstances, FlexibleInstances #-}

module Equivalence.TypeToGrammar
( convertToGrammar
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Duality
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Syntax.Show
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
  Grammar (substitute θ word) (substitute θ (productions state))
  where
    (word, state) = runState (mapM typeToGrammar ts) (initial tEnv)
    θ = substitution state

typeToGrammar :: Type -> TransState Word
typeToGrammar t = do
  collect [] u
  toGrammar u
  where u = normalise Map.empty t -- TODO: use a simpler unravel function

toGrammar :: Type -> TransState Word
-- Non rec-types
toGrammar (Skip _) =
  return []
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar m@(Message _ _ _) = do
  y <- getLHS $ Map.singleton (show m)  []
  return [y]
toGrammar (Choice _ v m) = do
  ms <- tMapM toGrammar m
  y <- getLHS $ Map.mapKeys (\k -> showChoiceView v ++ show k) ms
  return [y]
-- Recursive types
toGrammar x@(TypeVar _ _) = do      -- x is a polymorphic variable
  y <- getLHS $ Map.singleton (show x) []
  return [y]
toGrammar (Rec _ (TypeVarBind _ x _) _) =
  return [x]
  -- Type operators
toGrammar (Dualof _ t) = toGrammar (dual t)
toGrammar (TypeName p x) = toGrammar (TypeVar p x) -- TODO: can a TypeName be taken as a TypeVar?
  -- Error (debugging)
toGrammar t = error $ "Internal error. Attempting to convert type " ++ show t ++ " (a non session type) to grammar."

type SubstitutionList = [(Type, TypeVar)]

collect :: SubstitutionList -> Type -> TransState ()
collect σ (Semi _ t u) = collect σ t >> collect σ u
collect σ (Choice _ _ m) = tMapM_ (collect σ) m
collect σ t@(Rec _ (TypeVarBind _ x _) u) = do
  let σ' = (t, x) : σ
  let u' = Substitution.subsAll σ' u
  (z:zs) <- toGrammar (normalise Map.empty u') -- TODO: use a simpler unravel function
  m <- getTransitions z
  addProduction x (Map.map (++ zs) m)
  -- addProductions x (Map.map (++ zs) m)
  collect σ' u
collect _ _ = return ()

-- The state of the translation to grammars

type Substitution = Map.Map TypeVar TypeVar

type TransState = State TState

data TState = TState {
  productions  :: Productions
, nextIndex    :: Int
, typeEnv      :: TypeEnv
, substitution :: Substitution
}

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions  = Map.empty
, nextIndex    = 1
, typeEnv      = tEnv
, substitution = Map.empty
}

getFreshVar :: TransState TypeVar
getFreshVar = do
  s <- get
  let n = nextIndex s
  modify $ \s -> s {nextIndex = n + 1}
  return $ mkVar defaultPos ("#X" ++ show n)

getProductions :: TransState Productions
getProductions = do
  s <- get
  return $ productions s

getTransitions :: TypeVar -> TransState Transitions
getTransitions x = do
  ps <- getProductions
  return $ ps Map.! x

getSubstitution :: TransState Substitution
getSubstitution = do
  s <- get
  return $ substitution s

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

-- addProduction :: TypeVar -> Label -> Word -> TransState ()
-- addProduction x l w =
--   modify $ \s -> s {productions = insertProduction (productions s) x l w}

addSubstitution :: TypeVar -> TypeVar -> TransState ()
addSubstitution x y =
  modify $ \s -> s {substitution = Map.insert x y (substitution s)}

-- Get the LHS for given transitions; if no productions for the
-- transitions are found, add a new productions and return their LHS
getLHS :: Transitions -> TransState TypeVar
getLHS ts = do
  ps <- getProductions
  case reverseLookup ts ps of
    Nothing -> do
      y <- getFreshVar
      addProductions y ts
      return y
    Just x ->
      return x
  where
    -- Lookup a key for a value in the map. Probably O(n)
    reverseLookup :: Eq a => Ord k => a -> Map.Map k a -> Maybe k
    reverseLookup a = Map.foldrWithKey (\k b acc -> if a == b then Just k else acc) Nothing

-- Adding a new production, but only if needed

addProduction :: TypeVar -> Transitions -> TransState ()
addProduction x ts = do
  ps <- getProductions
  reverseLookup' x ts ps >>= \case
    False -> addProductions x ts >> return ()
    True  -> return ()

reverseLookup' :: TypeVar -> Transitions -> Productions -> TransState Bool
reverseLookup' x a =
  Map.foldrWithKey
    (\k b acc -> compareTrans x k a b >>= \b -> if b then return True else acc)
    (return False)

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
                                   (compareWords w (ts2 Map.! l) s)) Set.empty ts1
      b <- fixedPoint s res x ts1
      if b && not (null res) -- TODO: new fun on where
        then addSubstitution x y >> return True
        else return False
  | otherwise = return False

-- Are two transitions equal?
-- Do they have the keys and the corresponding words are of the same size?
equalTrans :: Transitions -> Transitions -> Bool
equalTrans ts1 ts2 =
  Map.keys ts1 == Map.keys ts2 &&
  (all (\(x,y) -> length x == length y) (zip (Map.elems ts1) (Map.elems ts2)))

fixedPoint :: VisitedProds -> VisitedProds -> TypeVar -> Transitions -> TransState Bool
fixedPoint visited goals w t
  | Set.null goals = return True
  | otherwise      = do
      let (x, y) = Set.elemAt 0 goals
      ps <- getProductions
      if Map.member y ps
      then do
        let ts1 = Map.findWithDefault t x ps
        θ <- getSubstitution
        let y' = substitute θ y
        ts2 <- getTransitions y'
        fixedPoint' (Set.insert (x,y) visited) (updateGoals goals (newGoals ts1 ts2) x y) ts1 ts2 x y'
      else return False
   where
     -- Recursively calls fixedPoint when the transitions are equal
     fixedPoint' :: VisitedProds -> Goals -> Transitions -> Transitions ->
                    TypeVar -> TypeVar -> TransState Bool
     fixedPoint' visited goals ts1 ts2 x y
       | equalTrans ts1 ts2 =
          fixedPoint (Set.insert (x,y) visited) (updateGoals goals (newGoals ts1 ts2) x y) w t
       | otherwise = return False

     newGoals :: Transitions -> Transitions -> Goals
     newGoals ts1 ts2 =
       Map.foldrWithKey (\l w acc -> acc `Set.union`
                             compareWords (ts1 Map.! l) w visited) Set.empty ts2

-- Deletes the current goal and updates it with the new ones
updateGoals :: Goals -> Goals -> TypeVar -> TypeVar -> Goals
updateGoals goals newGoals x y = Set.union newGoals (Set.delete (x, y) goals)

-- Compares two words
-- If they are on the Set of visited productions, there is no need
-- to visit them. Otherwise, we add them to the set of productions
-- that we still need to explore.
compareWords ::  Word -> Word -> VisitedProds ->ToVisitProds
compareWords xs ys s =
      foldl (\acc (x, y) -> if x == y || Set.member (x,y) s
                            then acc
                            else Set.insert (x, y) acc ) Set.empty (zip xs ys)

-- Apply a TypeVar/TypeVar substitution to different objects

class Substitute t where
  substitute :: Substitution -> t -> t

instance Substitute TypeVar where
  substitute θ v = Map.foldrWithKey (\x y w -> if x == w then y else w) v θ

instance Substitute Word where
  substitute θ = map (substitute θ)

instance Substitute [Word] where
  substitute θ = map (substitute θ)

instance Substitute Transitions where
  substitute θ = Map.map (substitute θ)

instance Substitute Productions where
  substitute θ = Map.map (substitute θ)
