
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

{-# LANGUAGE NoMonadFailDesugaring, FlexibleInstances #-}

module Equivalence.TypeToGrammar
  ( convertToGrammar
  )
where

import           Bisimulation.Grammar
import           Parse.Unparser
import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import qualified Validation.Substitution       as Substitution
                                                ( subsAll
                                                , unfold
                                                ) -- no renaming
-- import           Equivalence.Normalisation
import           Utils.FreestState              ( tMapM
                                                , tMapM_
                                                )
import           Utils.Errors                   ( internalError )
import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Prelude                 hiding ( Word ) -- Word is (re)defined in module Equivalence.Grammar
import           Validation.Terminated

-- Conversion to context-free grammars

convertToGrammar :: T.TypeEnv -> [T.Type] -> Grammar
convertToGrammar tEnv ts = --trace ("subs: " ++ show (subs state))  $
                           Grammar (substitute θ word)
                                   (substitute θ (productions state))
 where
  (word, state) = runState (mapM typeToGrammar ts) (initial tEnv)
  θ             = substitution state

typeToGrammar :: T.Type -> TransState Word
typeToGrammar t = do
  collect [] u
  toGrammar u
  where u = t
  -- where u = unr t -- TODO: TACAS does not unravel here...
--   where u = normalise Map.empty t -- TODO: use a simpler unravel function

toGrammar :: T.Type -> TransState Word
-- Non rec-types
toGrammar (T.Skip _    ) = return []
toGrammar (T.Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar m@T.Message{} = do
  y <- getLHS $ Map.singleton (show m) []
  return [y]
toGrammar (T.Choice _ v m) = do
  ms <- tMapM toGrammar m
  y  <- getLHS $ Map.mapKeys (\k -> showChoiceView v ++ show k) ms
  return [y]
-- Recursive types
toGrammar x@T.Var{} = do      -- x is a polymorphic variable
  y <- getLHS $ Map.singleton (show x) []
  return [y]
toGrammar (T.Rec _ (K.Bind _ x _) _) = return [x]
toGrammar t = internalError "Equivalence.TypeToGrammar.toGrammar" t

type SubstitutionList = [(T.Type, TypeVar)]

collect :: SubstitutionList -> T.Type -> TransState ()
collect σ (  T.Semi   _ t              u) = collect σ t >> collect σ u
collect σ (  T.Choice _ _              m) = tMapM_ (collect σ) m
collect σ t@(T.Rec    _ (K.Bind _ x _) u) = do
  let σ' = (t, x) : σ
  let u' = Substitution.subsAll σ' u
  (z : zs) <- toGrammar (unr u') -- TODO: use a simpler unravel function
  -- (z:zs) <- toGrammar (normalise Map.empty u') -- TODO: use a simpler unravel function
  m        <- getTransitions z
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
, typeEnv      :: T.TypeEnv
, substitution :: Substitution
}

-- State manipulating functions, get and put

initial :: T.TypeEnv -> TState
initial tEnv = TState { productions  = Map.empty
                      , nextIndex    = 1
                      , typeEnv      = tEnv
                      , substitution = Map.empty
                      }

getFreshVar :: TransState TypeVar
getFreshVar = do
  s <- get
  let n = nextIndex s
  modify $ \s -> s { nextIndex = n + 1 }
  return $ mkVar defaultPos ("#X" ++ show n)

getProductions :: TransState Productions
getProductions = gets productions

getTransitions :: TypeVar -> TransState Transitions
getTransitions x = do
  ps <- getProductions
  return $ ps Map.! x

getSubstitution :: TransState Substitution
getSubstitution = gets substitution

putProductions :: TypeVar -> Transitions -> TransState ()
putProductions x m =
  modify $ \s -> s { productions = Map.insert x m (productions s) }

-- putProduction :: TypeVar -> Label -> Word -> TransState ()
-- putProduction x l w =
--   modify $ \s -> s {productions = insertProduction (productions s) x l w}

putSubstitution :: TypeVar -> TypeVar -> TransState ()
putSubstitution x y =
  modify $ \s -> s { substitution = Map.insert x y (substitution s) }

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
    Just x -> return x
 where
    -- Lookup a key for a value in the map. Probably O(n)
  reverseLookup :: Eq a => Ord k => a -> Map.Map k a -> Maybe k
  reverseLookup a =
    Map.foldrWithKey (\k b acc -> if a == b then Just k else acc) Nothing

-- Add new productions, but only if needed

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x ts = do
  ps <- getProductions
  b  <- existProductions x ts ps
  unless b (putProductions x ts)

existProductions :: TypeVar -> Transitions -> Productions -> TransState Bool
-- existProductions x ts _ = return False
existProductions x ts = Map.foldrWithKey
  (\x' ts' acc -> sameTrans x x' ts ts' >>= \b -> if b then return True else acc
  )
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
    let res = Map.foldrWithKey
          (\l w acc -> acc `Set.union` compareWords w (ts2 Map.! l) s)
          Set.empty
          ts1
    b <- fixedPoint s res ts1
    if b && not (null res) -- TODO: new fun on where
      then putSubstitution x1 x2 >> return True
      else return False
  | otherwise = return False

-- Are two transitions equal?  Do they have the same keys and the
-- corresponding words are of the same size?
matchingTrans :: Transitions -> Transitions -> Bool
matchingTrans ts1 ts2 = Map.keys ts1 == Map.keys ts2 && all
  (\(x, y) -> length x == length y)
  (zip (Map.elems ts1) (Map.elems ts2))

-- Compares two words
-- If they are on the Set of visited productions, there is no need
-- to visit them. Otherwise, we add them to the set of productions
-- that we still need to explore.
compareWords :: Word -> Word -> VisitedProds -> ToVisitProds
compareWords xs ys visited = foldr
  (\p@(x, y) acc ->
    if x == y || p `Set.member` visited then acc else Set.insert p acc
  )
  Set.empty
  (zip xs ys)

fixedPoint :: VisitedProds -> VisitedProds -> Transitions -> TransState Bool
fixedPoint visited goals ts
  | Set.null goals = return True
  | otherwise = do
    let goal@(x, y) = Set.elemAt 0 goals
    ps <- getProductions
    if y `Map.member` ps
      then do
        let ts1 = Map.findWithDefault ts x ps
        θ <- getSubstitution
        let y' = substitute θ y
        ts2 <- getTransitions y'
        if matchingTrans ts1 ts2
          then
            let newVisited = Set.insert (x, y') visited
                newGoals   = Set.delete goal goals `Set.union` moreGoals ts1 ts2
            in  fixedPoint newVisited newGoals ts
          else return False
      else return False
 where
  moreGoals :: Transitions -> Transitions -> Goals
  moreGoals ts1 = Map.foldrWithKey
    (\l xs acc -> acc `Set.union` compareWords (ts1 Map.! l) xs visited)
    Set.empty

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

-- Unravel

unr :: T.Type -> T.Type
unr t@T.Rec{} = unr (Substitution.unfold t)
-- unr (T.Semi p t1 t2) | unr t1 == T.Skip p = unr t2
unr (T.Semi p t1 t2) | terminated t1 = unr t2
                     | otherwise     = T.Semi p (unr t1) t2
unr t = t
