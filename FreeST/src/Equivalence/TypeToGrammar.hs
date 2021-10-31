{- |
Module      :  Equivalence.TypeToGrammar
Description :  Converting types to grammars
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module converts a list of session types into a grammar.
-}

{-# LANGUAGE FlexibleInstances #-}

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
                                                ( subsAll )
import           Equivalence.Normalisation      ( normalise )
import           Util.Error                     ( internalError )
import           Util.FreestState               ( tMapM
                                                , tMapM_
                                                )
import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Prelude                       hiding ( Word ) -- Word is (re)defined in module Bisimulation.Grammar
import           Debug.Trace

-- Conversion to simple grammars

convertToGrammar :: [T.Type] -> Grammar
convertToGrammar ts = trace (show ts ++ "\n" ++ show grammar) grammar
  where
    grammar = Grammar (substitute θ word) (substitute θ (productions state))
    (word, state) = runState (mapM typeToGrammar ts) initial
    θ             = substitution state

typeToGrammar :: T.Type -> TransState Word
typeToGrammar t = do
  collect [] t
  toGrammar t

toGrammar :: T.Type -> TransState Word
-- Syntactic equality
toGrammar t = case fatTerminal t of
  Just t' ->  getLHS $ Map.singleton (show t') []
  Nothing -> toGrammar' t

toGrammar' :: T.Type -> TransState Word
-- Functional Types
toGrammar' (T.Arrow _ p t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  getLHS $ Map.fromList [(left $ showArrow p, xs), (right $ showArrow p, ys)]
toGrammar' (T.Pair _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  getLHS $ Map.fromList [(left "*", xs), (right "*", ys)]
toGrammar' (T.Variant _ m) = do -- Can't test this type directly
  ms <- tMapM toGrammar m
  getLHS $ Map.mapKeys (\k -> "<>" ++ show k) ms
-- Session Types
toGrammar' (T.Skip _) = return []
toGrammar' (T.Semi _ t u) = liftM2 (++) (toGrammar t) (toGrammar u)
toGrammar' (T.Message _ p t) = do
  xs <- toGrammar t
  b <- getBottom
  getLHS $ Map.fromList [(left $ show p, xs ++ [b]), (right $ show p, [])]
toGrammar' (T.Choice _ v m) = do
  ms <- tMapM toGrammar m
  getLHS $ Map.mapKeys (\k -> showChoiceView v ++ show k) ms
-- Polymorphism and recursive types
-- toGrammar' t@T.Forall{} =
toGrammar' (T.Rec _ (K.Bind _ x _ _)) = return [x]
toGrammar' t@T.Var{} = getLHS $ Map.singleton (show t) []
-- Type operators
toGrammar' t@T.CoVar{} = getLHS $ Map.singleton (show t) []
-- toGrammar' t@T.Dualof{} =
toGrammar' t = internalError "Equivalence.TypeToGrammar.toGrammar" t

left :: String -> String
left = (++ "l")

right :: String -> String
right = (++ "r")

-- Fat terminal types can be compared for syntactic equality
-- Returns a normalised type in case the type can become fat terminal
fatTerminal :: T.Type -> Maybe T.Type
-- Functional Types
fatTerminal t@T.Int{}             = Just t
fatTerminal t@T.Char{}            = Just t
fatTerminal t@T.Bool{}            = Just t
fatTerminal t@T.String{}          = Just t
fatTerminal t@T.Unit{}            = Just t
fatTerminal (T.Arrow p m t u)     = Just (T.Arrow p m) <*> fatTerminal t <*> fatTerminal u
fatTerminal (T.Pair p t u)        = Just (T.Pair p) <*> fatTerminal t <*> fatTerminal u
fatTerminal (T.Variant p m)       = Just (T.Variant p) <*> mapM fatTerminal m
-- Session Types
fatTerminal (T.Semi _ T.Skip{} t) = fatTerminal t
fatTerminal (T.Semi _ t T.Skip{}) = fatTerminal t
fatTerminal (T.Message p pol t)   = Just (T.Message p pol) <*> fatTerminal t
-- These two would preclude distributivity:
-- fatTerminal (T.Semi p t u)      = Just (T.Semi p) <*> fatTerminal t <*> fatTerminal u
-- fatTerminal (T.Choice p pol m)  = Just (T.Choice p pol) <*> mapM fatTerminal m
-- Default
fatTerminal _                     = Nothing

{-
-- Can this type become a fat terminal?
syntactic :: T.Type -> Bool
-- Functional Types
syntactic t@T.Int{}             = True
syntactic t@T.Char{}            = True
syntactic t@T.Bool{}            = True
syntactic t@T.String{}          = True
syntactic t@T.Unit{}            = True
syntactic (T.Arrow _ _ t u)     = syntactic t && syntactic u
syntactic (T.Pair _ t u)        = syntactic t && syntactic u
syntactic (T.Variant p m)       = Map.foldr (\t b -> b && syntactic t) True m
-- Session Types
syntactic (T.Semi _ T.Skip{} t) = syntactic t
syntactic (T.Semi _ t T.Skip{}) = syntactic t
syntactic (T.Message _ _ t)     = syntactic t
-- These two would preclude distributivity:
-- syntactic (T.Semi p t u)      = 
-- syntactic (T.Choice p pol m)  = 
-- Default
syntactic _                     = False
-}

type SubstitutionList = [(T.Type, TypeVar)]

collect :: SubstitutionList -> T.Type -> TransState ()
collect σ (T.Semi _ t u) = collect σ t >> collect σ u
collect σ (T.Choice _ _ m) = tMapM_ (collect σ) m
collect σ (T.Message _ _ t) = collect σ t
collect σ t@(T.Rec _ (K.Bind _ x _ u)) = do
  let σ' = (t, x) : σ
  let u' = Substitution.subsAll σ' u
  ~(z : zs) <- toGrammar (normalise u')
  m         <- getTransitions z
  addProductions x (Map.map (++ zs) m)
  collect σ' u
collect σ (T.Arrow _ _ t u) = collect σ t >> collect σ u
collect σ (T.Pair _ t u) = collect σ t >> collect σ u
collect _ _ = return ()

-- The state of the translation to grammar

type Substitution = Map.Map TypeVar TypeVar

type TransState = State TState

data TState = TState {
  productions  :: Productions
, nextIndex    :: Int
, substitution :: Substitution
, bottom       :: TypeVar
}

-- State manipulating functions, get and put

initial :: TState
initial = TState { productions  = Map.empty
                 , nextIndex    = 1
                 , substitution = Map.empty
                 , bottom       = makeFreshVar 0 -- smaller than the nextIndex
                 }

getFreshVar :: TransState TypeVar
getFreshVar = do
  s <- get
  let n = nextIndex s
  modify $ \s -> s { nextIndex = n + 1 }
  return $ makeFreshVar n

makeFreshVar :: Int -> TypeVar
makeFreshVar n = mkVar defaultPos ("#X" ++ show n)

getProductions :: TransState Productions
getProductions = gets productions

getBottom :: TransState TypeVar
getBottom = gets bottom

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
-- transitions are found, add a new production and return its LHS
getLHS :: Transitions -> TransState Word
getLHS ts = do
  ps <- getProductions
  case reverseLookup ts ps of
    Nothing -> do
      y <- getFreshVar
      putProductions y ts
      return [y]
    Just x -> return [x]
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
  substitute = map . substitute

instance Substitute [Word] where
  substitute = map . substitute

instance Substitute Transitions where
  substitute = Map.map . substitute

instance Substitute Productions where
  substitute = Map.map . substitute
