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
  addProductions x (Map.map (++ zs) m)
  -- putProductions x (Map.map (++ zs) m)
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

-- State manipulating functions, get and put

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

putProductions :: TypeVar -> Transitions -> TransState ()
putProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

-- putProduction :: TypeVar -> Label -> Word -> TransState ()
-- putProduction x l w =
--   modify $ \s -> s {productions = insertProduction (productions s) x l w}

putSubstitution :: TypeVar -> TypeVar -> TransState ()
putSubstitution x y =
  modify $ \s -> s {substitution = Map.insert x y (substitution s)}

-- Get the LHS for given transitions; if no productions for the
-- transitions are found, add a new productions and return their LHS
getLHS :: Transitions -> TransState TypeVar
getLHS ts = do
  ps <- getProductions
  case reverseLookup ts ps of
    Nothing -> do
      y <- getFreshVar
      putProductions y ts
      return y
    Just x ->
      return x
  where
    -- Lookup a key for a value in the map. Probably O(n)
    reverseLookup :: Eq a => Ord k => a -> Map.Map k a -> Maybe k
    reverseLookup a = Map.foldrWithKey (\k b acc -> if a == b then Just k else acc) Nothing

-- Add new productions, but only if needed

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x ts = do
  ps <- getProductions
  existProductions x ts ps >>= \case
    True  -> return ()
    False -> putProductions x ts >> return ()

existProductions :: TypeVar -> Transitions -> Productions -> TransState Bool
existProductions x ts =
  Map.foldrWithKey
    (\x' ts' acc -> sameTrans x x' ts ts' >>= \b -> if b then return True else acc)
    (return False)

-- TODO: Change these names
-- These are two different concepts
type VisitedProds = Set.Set (TypeVar, TypeVar)
type Goals = Set.Set (TypeVar, TypeVar)
type ToVisitProds = Set.Set (TypeVar, TypeVar)

sameTrans :: TypeVar -> TypeVar -> Transitions -> Transitions -> TransState Bool
sameTrans x1 x2 ts1 ts2
  | matchingTrans ts1 ts2 = do
      let s = Set.singleton (x1, x2)
      let res = Map.foldrWithKey (\l w acc -> acc `Set.union`
                                   (compareWords w (ts2 Map.! l) s)) Set.empty ts1
      b <- fixedPoint s res ts1
      if b && not (null res) -- TODO: new fun on where
        then putSubstitution x1 x2 >> return True
        else return False
  | otherwise = return False

-- Are two transitions equal?  Do they have the same keys and the
-- corresponding words are of the same size?
matchingTrans :: Transitions -> Transitions -> Bool
matchingTrans ts1 ts2 =
  Map.keys ts1 == Map.keys ts2 &&
  (all (\(x,y) -> length x == length y) (zip (Map.elems ts1) (Map.elems ts2)))

-- Compares two words
-- If they are on the Set of visited productions, there is no need
-- to visit them. Otherwise, we add them to the set of productions
-- that we still need to explore.
compareWords ::  Word -> Word -> VisitedProds -> ToVisitProds
compareWords xs ys visited = foldr
  (\p@(x, y) acc -> if x == y || p `Set.member` visited then acc else Set.insert p acc)
  Set.empty
  (zip xs ys)

fixedPoint :: VisitedProds -> VisitedProds -> Transitions -> TransState Bool
fixedPoint visited goals ts
  | Set.null goals = return True
  | otherwise      = do
      let goal@(x, y) = Set.elemAt 0 goals
      ps <- getProductions
      if y `Map.member` ps
      then do
        let ts1 = Map.findWithDefault ts x ps
        θ <- getSubstitution
        let y' = substitute θ y
        ts2 <- getTransitions y'
        if matchingTrans ts1 ts2
        then let
          newVisited = Set.insert goal (Set.insert (x, y') visited)
          newGoals = Set.delete goal goals `Set.union` moreGoals ts1 ts2
          in fixedPoint newVisited newGoals ts
        else return False
      else return False
    where
      moreGoals :: Transitions -> Transitions -> Goals
      moreGoals ts1 ts2 = Map.foldrWithKey
        (\l w acc -> acc `Set.union` compareWords (ts1 Map.! l) w visited)
        Set.empty
        ts2

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
