{-# LANGUAGE FlexibleContexts #-}
module Restriction.Utils where

import           Syntax.AST
import           Syntax.Base
import           Util.State
import qualified Restriction.Restriction as R
import qualified Syntax.Type as T

import qualified Control.Monad.State as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

getInequalities :: S.MonadState (FreestS a) m => m Inequalities
getInequalities = S.gets inequalities

addInequality :: S.MonadState (FreestS a) m => Span -> R.Inequality -> m ()
addInequality span inequality = S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span inequality "null" (-1)) (inequalities s) })

addFullInequality :: S.MonadState (FreestS a) m => Span -> R.Inequality -> String -> Int -> m ()
addFullInequality span inequality function threadNum = 
  S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span inequality function threadNum) (inequalities s) })

addInequalities :: S.MonadState (FreestS a) m => Span -> T.Level -> ContextSet -> m ()
addInequalities span l1 ctx = mapM_ (\l2 -> addInequality span (l1, l2)) (Set.toList ctx)

addInequalities2 :: S.MonadState (FreestS a) m => Span -> T.Level -> [T.Level] -> m ()
addInequalities2 span l1 = mapM_ (\l2 -> addInequality span (l1, l2))

addInequalitiesInReverse :: S.MonadState (FreestS a) m => Span -> T.Level -> [T.Level] -> m ()
addInequalitiesInReverse span l1 ls = do
  mapM_ (\l2 -> addInequality span (l2, l1)) ls

getEqualities :: S.MonadState (FreestS a) m => m Equalities
getEqualities = S.gets equalities

addEquality :: S.MonadState (FreestS a) m => Span -> R.Equality -> String -> Int -> m ()
addEquality span equality function threadNum =
  S.modify (\s -> s { equalities = Set.insert (R.EqualityEntry span equality function threadNum) (equalities s) })

getContextStack :: S.MonadState (FreestS a) m => m [ContextSet]
getContextStack = S.gets context

getContext :: S.MonadState (FreestS a) m => m ContextSet
getContext = do
  ctx <- S.gets context
  case ctx of
    (x:_) -> return x
    []      -> return Set.empty

getGlobalContext :: S.MonadState (FreestS a) m => m ContextSet
getGlobalContext = do
  gctx <- S.gets globalContext
  ctx <- getContext
  ctxStack <- getContextStack
  if gctx == Set.empty && length ctxStack == 1
    then do
      S.modify (\s -> s { globalContext = ctx })
      return ctx
    else return gctx

resetGlobalContext :: S.MonadState (FreestS a) m => m ()
resetGlobalContext = do
  S.modify (\s -> s { globalContext = Set.empty })
  S.modify (\s -> s { firstInContext = T.Top })

updateContext :: S.MonadState (FreestS a) m => T.Level -> m ()
updateContext l = do
  ctxStack <- getContextStack
  case ctxStack of
    (x:xs) -> do
      let newTop = Set.insert l x
      S.modify (\s -> s { context = newTop : xs })
      if x == Set.empty 
        then do
          S.modify (\s -> s { firstInContext = if firstInContext s == T.Top then l else firstInContext s })
          S.modify (\s -> s { latestInContext = l })
        else do
          S.modify (\s -> s { firstInContext = firstInContext s })
          S.modify (\s -> s { latestInContext = l })
    [] -> do
      gctx <- getGlobalContext
      if gctx == Set.empty
        then do
          S.modify (\s -> s { globalContext = Set.singleton l })
          S.modify (\s -> s { firstInContext = if firstInContext s == T.Top then l else firstInContext s })
          S.modify (\s -> s { latestInContext = l })
          pushContext l
        else pushContext l

newContext :: S.MonadState (FreestS a) m => m ()
newContext = S.modify (\s -> s { context = Set.empty : context s })

pushContext :: S.MonadState (FreestS a) m => T.Level -> m ()
pushContext l = S.modify (\s -> s { context = Set.singleton l : context s })

popContext :: S.MonadState (FreestS a) m => m ()
popContext = do
  ctxStack <- getContextStack
  case ctxStack of
    (x:xs) -> do
      gctx <- getGlobalContext
      S.modify (\s -> s { globalContext = Set.union x gctx })
      S.modify (\s -> s { context = xs })
    [] -> do
      S.modify (\s -> s { context = [] })

getFirstInContext :: S.MonadState (FreestS a) m => m T.Level
getFirstInContext = S.gets firstInContext

checkRenamedContext :: S.MonadState (FreestS a) m => T.Level -> m T.Level
checkRenamedContext l = do
  pc <- S.gets polyContext
  if pc /= T.Top
    then return pc
    else return l

setFirstInContext :: S.MonadState (FreestS a) m => T.Level -> m ()
setFirstInContext l = do
  fic <- S.gets firstInContext
  if fic == T.Top
    then S.modify (\s -> s { firstInContext = l })
    else return ()

setPolyContext :: S.MonadState (FreestS a) m => T.Level -> m ()
setPolyContext l = do
  pc <- S.gets polyContext
  if pc == T.Top
    then S.modify (\s -> s { polyContext = l })
    else return ()

clearFirstInContext :: S.MonadState (FreestS a) m => m ()
clearFirstInContext = do
  S.modify (\s -> s { firstInContext = T.Top })
  S.modify (\s -> s { polyContext = T.Top })

getLatestInContext :: S.MonadState (FreestS a) m => m T.Level
getLatestInContext = S.gets latestInContext

clearLatestInContext :: S.MonadState (FreestS a) m => m ()
clearLatestInContext = S.modify (\s -> s { latestInContext = T.Top })

incrementLevelVarCounter :: S.MonadState (FreestS a) m => m ()
incrementLevelVarCounter = do
  n <- S.gets levelVarCounter
  S.modify (\s -> s { levelVarCounter = n + 1 })

minLevel :: S.MonadState (FreestS a) m => Span -> [T.Level] -> m T.Level
minLevel span ls = do
  let (isEdgeVal, l') = checkMinTopBot ls
  if isEdgeVal
    then return l'
    else do
      n <- S.gets levelVarCounter
      let newLevel = T.LVar $ mkVar defaultSpan ("levelVar" ++ show n)
      incrementLevelVarCounter
      mapM_ (\l -> addInequality span (newLevel, l)) ls
      return newLevel

maxLevel :: S.MonadState (FreestS a) m => Span -> [T.Level] -> m T.Level
maxLevel span ls = do
  let (isEdgeVal, l') = checkMaxTopBot ls
  if isEdgeVal
    then return l'
    else do
      n <- S.gets levelVarCounter
      let newLevel = T.LVar $ mkVar defaultSpan ("levelVar" ++ show n)
      incrementLevelVarCounter
      mapM_ (\l -> addInequality span (l, newLevel)) ls
      return newLevel

checkMinTopBot :: [T.Level] -> (Bool, T.Level)
checkMinTopBot [] = (True, T.Top)
checkMinTopBot [x] = (True, x)
checkMinTopBot xs
  | any (== T.Bottom) xs = (True, T.Bottom)
  | all (== T.Top) xs = (True, T.Top)
  | length vars == 1 = (True, head vars)
  | T.Top `elem` xs && any isVar xs = (False, T.Top)
  | otherwise = (False, T.Top)
  where
    isVar T.Bottom = False
    isVar T.Top = False
    isVar _         = True
    vars = filter isVar xs

checkMaxTopBot :: [T.Level] -> (Bool, T.Level)
checkMaxTopBot [] = (True, T.Top)
checkMaxTopBot [x] = (True, x)
checkMaxTopBot xs
  | any (== T.Top) xs = (True, T.Top)
  | all (== T.Bottom) xs = (True, T.Bottom)
  | length vars == 1 = (True, head vars)
  | T.Bottom `elem` xs && any isVar xs = (False, T.Top)
  | otherwise = (False, T.Top)
  where
    isVar T.Bottom = False
    isVar T.Top = False
    isVar _         = True
    vars = filter isVar xs

levelOfTypeMap :: S.MonadState (FreestS a) m => Span -> T.TypeMap -> m T.Level
levelOfTypeMap span tm
  | Map.null tm = return T.Top
  | otherwise = do
      ls <- mapM l (Map.elems tm)
      minLevel span ls
  where
    l (T.Labelled _ T.Record _ m)  = levelOfTypeMap span m
    l (T.Labelled _ T.Variant _ m) = levelOfTypeMap span m
    l t                            = return (R.level t)

getTypeLevel :: S.MonadState (FreestS a) m => T.Type -> m T.Level
getTypeLevel t = do
  case t of
    T.Labelled _ T.Record _ m  -> levelOfTypeMap (getSpan t) m
    T.Labelled _ T.Variant _ m -> levelOfTypeMap (getSpan t) m
    _                          -> return (R.level t)

addFunctionCall :: S.MonadState (FreestS a) m => String -> m Int
addFunctionCall name = do
  m <- S.gets functionCalls
  let val = case Map.lookup name m of
                 Nothing -> 0
                 Just v  -> v + 1
  S.modify (\s -> s { functionCalls = Map.insert name val m })
  return val

getFunctionCallsOf :: S.MonadState (FreestS a) m => String -> m Int
getFunctionCallsOf name = do
  m <- S.gets functionCalls
  return $ Map.findWithDefault 0 name m

registerFunctionPositions :: S.MonadState (FreestS a) m => Definitions a -> m ()
registerFunctionPositions defs = do
  S.forM_ (Map.toList defs) $ \(k, v) -> do
    let span = getSpan k
    let (startingPos, _) = startPos span
    S.when (moduleName span /= "Prelude" && moduleName span /= "<default>") $ do
      S.modify (\s -> s { functionPositions = Map.insert (extern k) (startingPos, -1) (functionPositions s) })
  orderFunctionPositions

getFunctionPositions :: S.MonadState (FreestS a) m => m (Map.Map String (Int, Int))
getFunctionPositions = S.gets functionPositions

orderFunctionPositions :: S.MonadState (FreestS a) m => m ()
orderFunctionPositions = do
  m <- S.gets functionPositions
  let xs = sortOn (\(_, (start, _)) -> start) (Map.toList m)
      go [] = []
      go [(name, (start, _))] = [(name, (start, -1))]
      go ((name, (start, _)) : rest@((_, (nextStart, _)):_)) =
        (name, (start, nextStart - 1)) : go rest
      newMap = Map.fromList (go xs)
  S.modify (\s -> s { functionPositions = newMap })

isInFunction :: S.MonadState (FreestS a) m => String -> Span -> m Bool
isInFunction name span = do
  m <- getFunctionPositions
  return $ case Map.lookup name m of
    Just (start, end) -> do
      let (pos, _) = startPos span
      pos >= start && lesserThan pos end
    Nothing           -> False
    where
      lesserThan n1 n2 = n2 == -1 || n1 < n2

duplicateConstraintsInFunc :: S.MonadState (FreestS a) m => String -> Int -> m ()
duplicateConstraintsInFunc func ver = do
  ineqs <- getInequalities
  S.forM_ (Set.toList ineqs) $ \(R.InequalityEntry p (l1,l2) f n) -> do
        inFunc <- isInFunction func p
        S.when inFunc $ do
              if ver > 0
                then do
                  case (l1, l2) of
                    (T.LNum _, _) -> return ()
                    (_, T.LNum _) -> return ()
                    _             -> addFullInequality p (l1, l2) func (ver + 1)
                else do
                  S.modify (\s -> s { inequalities = Set.delete (R.InequalityEntry p (l1, l2) f n) (inequalities s) })
                  addFullInequality p (l1, l2) func (ver + 1)

pushLevelToAbstractionContext :: S.MonadState (FreestS a) m => T.Level -> m ()
pushLevelToAbstractionContext l = S.modify (\s -> s { abstractionContext = l : abstractionContext s })

popLevelFromAbstractionContext :: S.MonadState (FreestS a) m => m T.Level
popLevelFromAbstractionContext = S.state $ \s -> case abstractionContext s of
  []     -> (T.Top, s)
  (x:xs) -> (x, s { abstractionContext = xs, abstractionStack = x : abstractionStack s })

substituteAbstractionContext :: S.MonadState (FreestS a) m => Variable -> T.Level -> m ()
substituteAbstractionContext v l = S.modify $ \s ->
  s { abstractionContext = map (substLevel v l) (abstractionContext s) }
  where
    substLevel v l (T.LVar v')
      | v == v'   = l
      | otherwise = T.LVar v'
    substLevel v l (T.LAdd l1 l2) = T.LAdd (substLevel v l l1) (substLevel v l l2)
    substLevel _ _ l =  l

clearAbstractionStack :: S.MonadState (FreestS a) m => m ()
clearAbstractionStack = S.modify (\s -> s { abstractionStack = [] })