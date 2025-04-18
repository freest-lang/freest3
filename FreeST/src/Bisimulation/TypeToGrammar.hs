{- |
Module      :  Bisimulation.TypeToGrammar
Description :  Converting types to grammars
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module converts a list of session types into a simple grammar without
unreachable symbols
-}

{-# LANGUAGE FlexibleInstances #-}

module Bisimulation.TypeToGrammar
  ( convertToGrammar
  )
where

import           Syntax.Base
import qualified Syntax.Type as T
import           Bisimulation.Grammar
import           Elaboration.Replace ( changePos )
import           Typing.Normalisation ( normalise )
import qualified Typing.Substitution as Substitution ( subsAll )
import           Kinding.Terminated ( terminated )
import           Parse.Unparser ( showArrow )
import           Util.Error ( internalError )
import           Util.State ( tMapM, tMapM_)

import           Control.Monad.State
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Prelude hiding ( Word ) -- redefined in module Bisimulation.Grammar
import           Debug.Trace (trace)

convertToGrammar :: [T.Type] -> Grammar
convertToGrammar ts = {- trace (show ts ++ "\n" ++ show grammar) -} grammar
  where
    -- ts'           = mapM $ removeNames [] preludeNamingCtx (length preludeNamingCtx) ts
    (word, state) = runState (mapM typeToGrammar ts) initial
    θ             = substitution state
    prods         = substitute θ (productions state)
    grammar       = Grammar (substitute θ word) prods

typeToGrammar :: T.Type -> TransState Word
typeToGrammar t = collect [] t >> toGrammar t

toGrammar :: T.Type -> TransState Word
toGrammar t = case fatTerminal t of
  Just t' ->  getLHS $ Map.singleton (FatTerm $ show t') []
  Nothing -> toGrammar' t

-- Only non fat terminals
toGrammar' :: T.Type -> TransState Word
-- Functional Types
toGrammar' (T.Arrow _ m t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  getLHS $ Map.fromList $
    [(ArrowD, xs), (ArrowR, ys)] ++ [(Arrow1, []) | m == Lin]
toGrammar' (T.Labelled _  s m) = do -- Can't test this type directly
  ms <- tMapM toGrammar m
  getLHS $ Map.insert (Labelled s) [bottom] $ Map.mapKeys (Label s . intern) ms
-- Session Types
toGrammar' (T.Skip _)        = return []
toGrammar' (T.End _ p)       = getLHS $ Map.singleton (End p) [bottom]
toGrammar' (T.Semi _ t u)    = liftM2 (++) (toGrammar t) (toGrammar u)
toGrammar' (T.Message _ p t) = do
  xs <- toGrammar t
  getLHS $ Map.fromList [(MessageP p, xs ++ [bottom]), (MessageC p, [])]
-- Polymorphism and recursive types
-- Use intern to build the terminal for polymorphic variables (do not use show which gets the program-level variable
toGrammar' (T.Forall _ (Bind _ a k t)) = do
  xs <- toGrammar t
  getLHS $  Map.singleton (Forall (intern a) k) xs
toGrammar' (T.Rec _ (Bind _ a _ _)) = return [a]
toGrammar' (T.Var _ a) = getLHS $ Map.singleton (Var $ intern a) []
-- Type operators
toGrammar' t@(T.Dualof _ T.Var{}) = getLHS $ Map.singleton (Var $ show t) []
-- toGrammar' t@T.Dualof{} =
toGrammar' t = internalError "Equivalence.TypeToGrammar.toGrammar" t

-- Fat terminal types can be compared for syntactic equality
-- Returns a normalised type in case the type can become fat terminal
fatTerminal :: T.Type -> Maybe T.Type
-- Functional Types
fatTerminal t@T.Int{} = Just t
fatTerminal t@T.Float{} = Just t
fatTerminal t@T.Char{} = Just t
fatTerminal t@T.String{} = Just t
-- This one would preclude multiplicity subtyping (apply when disabled?)
-- fatTerminal (T.Arrow p m t u) =
--   Just (T.Arrow p m) <*> fatTerminal t <*> fatTerminal u
-- These two would preclude width subtyping (apply when disabled?)
-- fatTerminal (T.Labelled p T.Variant m) =
--   Just (T.Labelled p T.Variant) <*> mapM fatTerminal m
-- fatTerminal (T.Labelled p T.Record m) =
--   Just (T.Labelled p T.Record) <*> mapM fatTerminal m
-- Session Types
fatTerminal (T.Semi p t u) | terminated t = changePos p <$> fatTerminal u
                           | terminated u = changePos p <$> fatTerminal t
fatTerminal (T.Message p pol t) =
  Just (T.Message p pol) <*> fatTerminal t
-- These two would preclude distributivity:
-- fatTerminal (T.Semi p t u)      = Just (T.Semi p) <*> fatTerminal t <*> fatTerminal u
-- fatTerminal (T.Choice p pol m)  = Just (T.Choice p pol) <*> mapM fatTerminal m
-- Default
fatTerminal _ = Nothing

instance Show T.Sort where
  show T.Record = "{}"
  show T.Variant = "[]"
  show (T.Choice v) = show v

-- Collect productions

type SubstitutionList = [(T.Type, Variable)]

collect :: SubstitutionList -> T.Type -> TransState ()
  -- Functional Types
collect σ (T.Arrow _ _ t u) = collect σ t >> collect σ u
collect σ (T.Labelled _ _ m) = tMapM_ (collect σ) m
  -- Session Types
collect σ (T.Semi _ t u) = collect σ t >> collect σ u
collect σ (T.Message _ _ t) = collect σ t
  -- Polymorphism and recursive types
-- collect σ (T.Forall _ (Bind _ a _ t)) = collect σ t -- Needed? Correct?
collect σ t@(T.Rec _ (Bind _ a _ u)) = do
  let σ' = (t, a) : σ
  let u' = Substitution.subsAll σ' u
  ~(z : zs) <- toGrammar (normalise u')
  m         <- getTransitions z
  addProductions a (Map.map (++ zs) m)
  collect σ' u
collect _ _ = return ()

-- The state of the translation to grammar

type Substitution = Map.Map Variable Variable

type TransState = State TState

data TState = TState {
    productions  :: Productions
  , nextIndex    :: Int
  , substitution :: Substitution
  }

-- A non-terminal without productions, guaranteed

bottom :: Variable
bottom = mkVar defaultSpan "⊥"

-- State manipulating functions, get and put

initial :: TState
initial = TState {
    productions  = Map.empty
  , nextIndex    = 1
  , substitution = Map.empty
  }

getFreshVar :: TransState Variable
getFreshVar = do
  s <- get
  let n = nextIndex s
  modify $ \s -> s { nextIndex = n + 1 }
  return $ mkVar defaultSpan ("#X" ++ show n)

getProductions :: TransState Productions
getProductions = gets productions

getTransitions :: Variable -> TransState Transitions
getTransitions x = do
  ps <- getProductions
  return $ ps Map.! x

-- getSubstitution :: TransState Substitution
-- getSubstitution = gets substitution

putProductions :: Variable -> Transitions -> TransState ()
putProductions x m =
  modify $ \s -> s { productions = Map.insert x m (productions s) }

putSubstitution :: Variable -> Variable -> TransState ()
putSubstitution x y =
  modify $ \s -> s { substitution = Map.insert x y (substitution s) }

-- Get the LHS for given transitions; if no productions for the
-- transitions are found, add new productions and return its LHS
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

addProductions :: Variable -> Transitions -> TransState ()
addProductions x ts = do
  ps <- getProductions
  b  <- existProductions x ts ps
  unless b (putProductions x ts)

existProductions :: Variable -> Transitions -> Productions -> TransState Bool
-- existProductions x ts _ = return False
existProductions x ts = Map.foldrWithKey
  (\x' ts' acc -> sameTrans x x' ts ts' >>= \b -> if b then return True else acc)
  (return False)

sameTrans :: Variable -> Variable -> Transitions -> Transitions -> TransState Bool
sameTrans x1 x2 ts1 ts2
  | matchingTrans ts1 ts2 = do
    let s   = Set.singleton (x1, x2)
    let res = findGoals s ts1 ts2
    b <- fixedPoint s res ts1
    if not (null res) && b then putSubstitution x1 x2 $> True else return False
  | otherwise = return False

-- Are two transitions equal?  Do they have the same keys and the
-- corresponding words are of the same size?
matchingTrans :: Transitions -> Transitions -> Bool
matchingTrans ts1 ts2 = Map.keys ts1 == Map.keys ts2 && all
  (\(x, y) -> length x == length y)
  (zip (Map.elems ts1) (Map.elems ts2))

type VisitedProds = Set.Set (Variable, Variable)
type ToVisitProds = Set.Set (Variable, Variable)
type Goals = Set.Set (Variable, Variable)

-- Compares two words
-- If they are on the Set of visited productions, there is no need
-- to visit them. Otherwise, we add them to the set of productions
-- that we still need to explore.
compareWords :: Word -> Word -> VisitedProds -> ToVisitProds
compareWords xs ys visited = foldr
  (\p@(x, y) acc ->
    if x == y || p `Set.member` visited then acc else Set.insert p acc
  ) Set.empty (zip xs ys)

fixedPoint :: VisitedProds -> ToVisitProds -> Transitions -> TransState Bool
fixedPoint visited goals ts
  | Set.null goals = return True
  | otherwise = do
    let (x, y) = Set.elemAt 0 goals
    ps <- getProductions
    fixedPoint' (x, y) ps
      (Map.findWithDefault ts x ps) =<< getTransitions y
 where
  fixedPoint' goal@(_, y) ps ts1 ts2
    | y `Map.notMember` ps        = return False
    | not $ matchingTrans ts1 ts2 = return False
    | otherwise                   =
      let newVisited = Set.insert goal visited in
        fixedPoint newVisited
         (Set.delete goal goals `Set.union`
          findGoals newVisited ts1 ts2) ts

findGoals :: VisitedProds -> Transitions -> Transitions -> Goals
findGoals visited ts1 = Map.foldrWithKey
  (\l xs acc -> acc `Set.union` compareWords (ts1 Map.! l) xs visited)
  Set.empty

-- Apply a Variable/Variable substitution to different objects

class Substitute t where
  substitute :: Substitution -> t -> t

instance Substitute Variable where
  substitute θ v = Map.foldrWithKey (\x y w -> if x == w then y else w) v θ

instance Substitute Word where
  substitute = map . substitute

instance Substitute [Word] where
  substitute = map . substitute

instance Substitute Transitions where
  substitute = Map.map . substitute

instance Substitute Productions where
  substitute = Map.map . substitute
