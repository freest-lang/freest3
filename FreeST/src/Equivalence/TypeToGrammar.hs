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

{-# LANGUAGE FlexibleInstances #-}

module Equivalence.TypeToGrammar
  ( convertToGrammar
  )
where

import           Bisimulation.Grammar
import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import qualified Validation.Substitution       as Substitution
                                                ( subsAll )
import           Equivalence.Normalisation      ( normalise )
import           Util.FreestState               ( tMapM
                                                , tMapM_
                                                )
import           Util.Error                     ( internalError )
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Prelude                 hiding ( Word ) -- Word is (re)defined in module Bisimulation.Grammar
-- import           Parse.Unparser -- debug

-- Conversion to context-free grammars

convertToGrammar :: [T.Type] -> Grammar
convertToGrammar ts = -- trace ("types: " ++ show ts ++ "\n")  $
                      Grammar (substitute θ word)
                              (substitute θ (productions state))
 where
  (word, state) = runState (mapM typeToGrammar ts) initial
  θ             = substitution state

typeToGrammar :: T.Type -> TransState Word
typeToGrammar t = collect [] t >> toGrammar t

toGrammar :: T.Type -> TransState Word
toGrammar (T.Skip _    ) = return []
toGrammar (T.Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar t@T.Message{}    = typeTerminal t
toGrammar (T.Choice _ v m) = do
  ms <- tMapM toGrammar m
  getLHS $ Map.mapKeys (\k -> show v ++ show k) ms
toGrammar t@T.Var{}                  = typeTerminal t
toGrammar t@T.CoVar{}                = typeTerminal t
toGrammar (T.Rec _ (K.Bind _ x _ _)) = return [x]
toGrammar t = internalError "Equivalence.TypeToGrammar.toGrammar" t

typeTerminal :: T.Type -> TransState Word
typeTerminal = terminal . show

terminal :: Label -> TransState Word
terminal l = getLHS $ Map.singleton l []

type SubstitutionList = [(T.Type, TypeVar)]

collect :: SubstitutionList -> T.Type -> TransState ()
collect σ (  T.Semi   _ t u          ) = collect σ t >> collect σ u
collect σ (  T.Choice _ _ m          ) = tMapM_ (collect σ) m
collect σ t@(T.Rec _ (K.Bind _ x _ u)) = do
  let σ' = (t, x) : σ
  let u' = Substitution.subsAll σ' u
  ~(z : zs) <- toGrammar (normalise u')
  m         <- getTransitions z
  addProductions x (Map.map (++ zs) m)
  collect σ' u
collect _ _ = return ()

-- The state of the translation to grammars

type Substitution = Map.Map TypeVar TypeVar

type TransState = State TState

data TState = TState {
  productions  :: Productions
, nextIndex    :: Int
, substitution :: Substitution
}

-- State manipulating functions, get and put

initial :: TState
initial =
  TState { productions = Map.empty, nextIndex = 1, substitution = Map.empty }

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
  (\x' ts' acc -> sameTrans x x' ts ts' >>= \b -> if b then return True else acc
  )
  (return False)

sameTrans :: TypeVar -> TypeVar -> Transitions -> Transitions -> TransState Bool
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

type VisitedProds = Set.Set (TypeVar, TypeVar)
type ToVisitProds = Set.Set (TypeVar, TypeVar)
type Goals = Set.Set (TypeVar, TypeVar)

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
