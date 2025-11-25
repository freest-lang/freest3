{-# LANGUAGE NamedFieldPuns, FlexibleContexts, TypeFamilies #-}
module Util.State where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Kind as K
import           Syntax.Program
import           Syntax.MkName
import qualified Syntax.Type as T
import           Util.Error
import           Util.Warning
import qualified Restriction.Restriction as R

import qualified Control.Monad.State as S
import           Data.List ( intercalate, nub, sortOn, isPrefixOf, find )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import qualified Data.Traversable as Traversable
import           Data.Void
import           Debug.Trace
import           Data.Char (isAlphaNum, isDigit)

type Warnings = [WarningType]
type Errors = [ErrorType]
-- type Inequalities = Set.Set (Span, R.Inequality)
type Inequalities = Set.Set R.InequalityEntry
-- type Equalities = Set.Set (Span, R.Equality)
type Equalities = Set.Set R.EqualityEntry
type ContextSet = Set.Set T.Level
type InstantiatedContextSet = Set.Set (T.Level, Int)
type FunctionCallNum = Map.Map String Int
type EndpointPriorities = Map.Map (Variable, String, Int) (Int, Int)

data FunctionData = FunctionData
  { funcPosition :: (Int, Int),
    funcParams :: [String],
    funcParamIndex :: Int,
    functionCallNum :: Int
  } deriving (Show, Eq)

-- data EndpointPriorityData = EndpointPriorityData
--   { currentPriority :: Int,
--     increment :: Int
--     -- functionName :: String
--   } deriving (Show, Eq)

data FreestS a = FreestS
  { ast :: AST a
  , nextIndex :: Int
  , errors :: Errors
  , warnings :: Warnings
  , typenames :: TypeOpsEnv -- TODO: Remove with the new errors 
  , extra :: XExtra a
  , inequalities :: Inequalities
  , equalities :: Equalities
  , context :: [T.Level]
  , context' :: [InstantiatedContextSet]
  , globalContext :: T.Level
  , globalContext' :: ContextSet
  , firstInContext :: T.Level
  , levelVarCounter :: Int
  , firstInContext' :: T.Level
  , latestInContext :: T.Level
  , functionCalls :: FunctionCallNum
  , functionPositions :: Map.Map String FunctionData
  , polyContext :: T.Level
  , abstractionContext :: [T.Level]
  , abstractionStack :: [T.Level]
  , endpointPriorities :: EndpointPriorities
  , latestFreshEndpoints :: (Variable, Variable)
  , priorityInstantiations :: Map.Map String Int
  , globalPriorityInstantiations :: Map.Map String Int
  , calledFunctions :: Map.Map String Int
  }

type family XExtra a
type FreestState a = S.State (FreestS a)

void :: Void
void = error "Attempt to evaluate void"

-- | Initial state

initial :: XExtra a -> FreestS a
initial ext = FreestS {
    ast = initialAST
  , nextIndex = 0
  , errors = []
  , warnings = []
  , typenames = Map.empty
  , extra = ext
  , inequalities = Set.empty
  , equalities = Set.empty
  , context = []
  , context' = []
  , globalContext = T.Top
  , globalContext' = Set.empty
  , firstInContext = T.Top
  , levelVarCounter = 1000
  , firstInContext' = T.Top
  , latestInContext = T.Top
  , functionCalls = Map.empty
  , functionPositions = Map.empty
  , polyContext = T.Top
  , abstractionContext = []
  , abstractionStack = []
  , endpointPriorities = Map.empty
  , latestFreshEndpoints = (mkVar defaultSpan "", mkVar defaultSpan "")
  , priorityInstantiations = Map.empty
  , globalPriorityInstantiations = Map.empty
  , calledFunctions = Map.empty
  }

-- Dummy phase. This instance allows calling functions from a generic context
data Gen
type instance XExtra Gen  = Void

initialS :: FreestS Gen
initialS = FreestS {
    ast = initialAST
  , nextIndex = 0
  , errors = []
  , warnings = []
  , typenames = Map.empty
  , extra = void
  , inequalities = Set.empty
  , equalities = Set.empty
  , context = []
  , context' = []
  , globalContext = T.Top
  , globalContext' = Set.empty
  , firstInContext = T.Top
  , levelVarCounter = 0
  , firstInContext' = T.Top
  , latestInContext = T.Top
  , functionCalls = Map.empty
  , functionPositions = Map.empty
  , polyContext = T.Top
  , abstractionContext = []
  , abstractionStack = []
  , endpointPriorities = Map.empty
  , latestFreshEndpoints = (mkVar defaultSpan "", mkVar defaultSpan "")
  , priorityInstantiations = Map.empty
  , globalPriorityInstantiations = Map.empty
  , calledFunctions = Map.empty
  }

-- | AST

getAST :: FreestState a (AST a)
getAST = S.gets ast

-- | SIGNATURES

addToSignatures :: S.MonadState (FreestS a) m => Variable -> T.Type -> m ()
addToSignatures b t = S.modify (\s -> s{ast = addSignature b t (ast s)})

getEvalOrder :: S.MonadState (FreestS a) m => m [[Variable]]
getEvalOrder =  S.gets (evalOrder . ast)

addToEvalOrder :: S.MonadState (FreestS a) m => [Variable] -> m ()
addToEvalOrder xs = S.modify (\s -> s{ast = addEvalOrder xs (ast s)})

addToLastEvalOrder :: S.MonadState (FreestS a) m => [Variable] -> m ()
addToLastEvalOrder xs = S.modify (\s -> s{ast = addLastEvalOrder xs (ast s)})

resetEO :: FreestS a -> FreestS a 
resetEO s = s{ast=(ast s){evalOrder=[]}}

appendEOs :: FreestS a -> FreestS a -> FreestS a 
appendEOs s s' = s{ast=(ast s){evalOrder = evalOrder (ast s) ++ evalOrder (ast s')}}

prependEOs :: FreestS a -> FreestS a -> FreestS a 
prependEOs s s' = s{ast=(ast s){evalOrder = evalOrder (ast s') ++ evalOrder (ast s)}}

getSignatures ::  S.MonadState (FreestS a) m => m Signatures
getSignatures = S.gets (signatures . ast)

getSignaturesS ::  FreestS a -> Signatures
getSignaturesS = signatures . ast

getFromSignatures ::  S.MonadState (FreestS a) m => Variable -> m (Maybe T.Type)
getFromSignatures = (`fmap` getSignatures) . flip (Map.!?)  

setSignatures :: S.MonadState (FreestS a) m => Signatures -> m ()
setSignatures sigs = S.modify (\s -> s{ast = setSigs sigs (ast s)})

removeFromSignatures :: S.MonadState (FreestS a) m => Variable -> m ()
removeFromSignatures x = S.modify (\s -> s{ast = removeSig x (ast s)})

-- | TYPES

getTypes :: S.MonadState (FreestS a) m => m Types
getTypes =  S.gets (types . ast)

setTypes :: S.MonadState (FreestS a) m => Types -> m ()
setTypes types = S.modify(\s -> s{ast = setASTTypes types (ast s)})

getTypesS :: FreestS a -> Types
getTypesS =  types . ast

getFromTypes ::  S.MonadState (FreestS a) m => Variable -> m (Maybe (K.Kind, T.Type))
getFromTypes = (`fmap` getTypes) . flip (Map.!?)


addToDefinitions :: S.MonadState (FreestS a) m => Variable -> XDef a -> m ()
addToDefinitions x t = S.modify (\s -> s{ast = addDefinition x t (ast s)})

getFromDefinitions ::  S.MonadState (FreestS a) m => Variable -> m (Maybe (XDef a))
getFromDefinitions = (`fmap` getDefs) . flip (Map.!?)

getDefs :: S.MonadState (FreestS a) m => m (Definitions a)
getDefs =  S.gets (definitions . ast)

setDefs :: S.MonadState (FreestS a) m => Definitions a -> m ()
setDefs defs = S.modify (\s -> s{ast = setDefinitions defs (ast s)})

getDefsS :: FreestS a -> Definitions a
getDefsS =  definitions . ast

addToTypes :: S.MonadState (FreestS a) m => Variable -> K.Kind -> T.Type -> m ()
addToTypes x k t = S.modify (\s -> s{ast = addType x k t (ast s)})

-- | INDEX

getNextIndex :: S.MonadState (FreestS a) m => m Int
getNextIndex = do
  i <- S.gets nextIndex
  S.modify (\s -> s{nextIndex = i + 1})
  return i

-- | ERRORS

getErrors :: RunOpts -> FreestS a -> String
getErrors runOpts s = (intercalate "\n" . map f . take 10 . reverse . nub . errors) s
  where f = showError (isStylable runOpts) (Left $ runFilePath runOpts) (typenames s)

hasErrors :: FreestS a -> Bool
hasErrors = not . null . errors

addError :: S.MonadState (FreestS a) m => ErrorType -> m ()
addError e = S.modify (\s -> s { errors = e : errors s })
  
setErrors :: S.MonadState (FreestS a) m => Errors -> m ()
setErrors errors = S.modify (\s -> s { errors })
  
-- | WARNINGS

getWarnings :: RunOpts -> FreestS a -> String
getWarnings runOpts s = (intercalate "\n" . map f . take 10 . reverse . warnings) s
  where f = showWarnings (isStylable runOpts) (runFilePath runOpts) (typenames s)

hasWarnings :: FreestS a -> Bool
hasWarnings = not . null . warnings

addWarning :: S.MonadState (FreestS a) m => WarningType -> m ()
addWarning w = S.modify (\s -> s { warnings = w : warnings s })


-- | RUNOPTS, Move to other module ???

data RunOpts = RunOpts { runFilePath   :: FilePath
--                     , preludeFile   :: Maybe FilePath
                       , args          :: [String]
                       , mainFunction  :: Maybe Variable
                       , isStylable    :: Bool
                       , quietmode     :: Bool
                       , subtyping     :: Bool 
                       , subTimeout_ms :: Int 
                       } deriving Show

defaultOpts :: RunOpts
defaultOpts = RunOpts { runFilePath   = ""
--                    , preludeFile   = Just "Prelude.fst"
                      , args = []
                      , mainFunction  = Nothing
                      , isStylable    = True
                      , quietmode     = False
                      , subtyping     = True 
                      , subTimeout_ms = 6*10^4 -- 1min
                      }


isMainFlagSet :: RunOpts -> Bool
isMainFlagSet = isJust . mainFunction

getMain :: RunOpts -> Variable
getMain opts = fromMaybe mkMain maybeMain
  where maybeMain = mainFunction opts


-- | OTHER MODULE?

-- typeListToRcdType :: [(Variable, [T.Type])] -> T.TypeMap
-- typeListToRcdType []             = Map.empty
-- typeListToRcdType ((c, us) : ts) =
--   Map.insert c (T.Labelled (getSpan c) T.Record $ typesToMap 0 us) (typeListToRcdType ts)
--   where typesToMap n [] = Map.empty
--         typesToMap n (t : ts) = Map.insert (mkVar (getSpan t) $ show n) t (typesToMap (n+1) ts)


-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapM_ :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m ()
tMapM_ f m = S.void $ tMapM f m

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

tMapWithKeyM_ :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m ()
tMapWithKeyM_ f m = S.void $ tMapWithKeyM f m

-- | TYPENAMES

addTypeName :: S.MonadState (FreestS a) m => Span -> T.Type -> m ()
addTypeName p t = S.modify (\s -> s { typenames = Map.insert p t (typenames s) })

getTypeNames :: S.MonadState (FreestS a) m => m TypeOpsEnv
getTypeNames = S.gets typenames

findTypeName :: S.MonadState (FreestS a) m => Span -> T.Type -> m T.Type
findTypeName p t = Map.findWithDefault t p <$> getTypeNames

addDualof :: S.MonadState (FreestS a) m => T.Type -> m ()
addDualof d@(T.Dualof p t) = do
  tn <- getTypeNames
  case tn Map.!? getSpan t of
    Just (T.Dualof _ _) -> return ()
    Just u -> S.modify (\s -> s { typenames = Map.insert p (T.Dualof p u) tn })
    Nothing -> S.modify (\s -> s { typenames = Map.insert p d tn })
addDualof t = internalError "Util.State.addDualof" t

-- | Debug Function

debugM :: S.MonadState (FreestS a) m => String -> m ()
debugM err = do
  i <- getNextIndex
  traceM $ "\n" ++ show i ++ ". " ++ err ++ "\n"

-- | LEVELS

getInequalities :: S.MonadState (FreestS a) m => m Inequalities
getInequalities = S.gets inequalities

addInequality :: S.MonadState (FreestS a) m => Span -> R.Inequality -> m ()
addInequality span inequality = do
  let (x,y) = inequality
  xi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar x)
  yi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar y)
  -- let xi' = case xi of
  --       Just xi'' -> xi''
  --       Nothing   -> -1
  -- let yi' = case yi of
  --       Just yi'' -> yi''
  --       Nothing   -> -1
  func <- getCurrentFunction (fst $ startPos span)
  S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span inequality func 0 xi yi) (inequalities s) })

addFullInequality :: S.MonadState (FreestS a) m => Span -> R.Inequality -> String -> Int -> m ()
addFullInequality span inequality function threadNum = do
  let (x,y) = inequality
  xi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar x)
  yi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar y)
  -- let xi' = case xi of
  --       Just xi'' -> xi''
  --       Nothing   -> -1
  -- let yi' = case yi of
  --       Just yi'' -> yi''
  --       Nothing   -> -1
  S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span inequality function threadNum xi yi) (inequalities s) })

addFullInequality' :: S.MonadState (FreestS a) m => Span -> R.Inequality -> String -> Int -> Int -> Int -> m ()
addFullInequality' span inequality function threadNum xi yi = do
  S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span inequality function threadNum xi yi) (inequalities s) })

addInequalities :: S.MonadState (FreestS a) m => Span -> T.Level -> ContextSet -> m ()
addInequalities span l1 ctx = mapM_ (\l2 -> addInequality span (l1, l2)) (Set.toList ctx)

addInequalities2 :: S.MonadState (FreestS a) m => Span -> T.Level -> [T.Level] -> m ()
addInequalities2 span l1 = mapM_ (\l2 -> addInequality span (l1, l2))

addInequalitiesInReverse :: S.MonadState (FreestS a) m => Span -> T.Level -> [T.Level] -> m ()
addInequalitiesInReverse span l1 ls = do
  mapM_ (\l2 -> addInequality span (l2, l1)) ls

addInstantiatedInequalities :: S.MonadState (FreestS a) m => Span -> T.Level -> Int -> InstantiatedContextSet -> m ()
addInstantiatedInequalities span l i ctx = do
  func <- getCurrentFunction (fst $ startPos span)
  mapM_ (\(l2, yi') -> S.modify (\s -> s { inequalities = Set.insert (R.InequalityEntry span (l, l2) func 0 i yi') (inequalities s) }) ) (Set.toList ctx)

-- addInequalities :: S.MonadState (FreestS a) m => Span -> T.Level -> ContextSet -> String -> Int -> m ()
-- addInequalities span l1 ctx function threadNum =
--   mapM_ (\l2 -> addInequality span (R.Inequality l1 l2) function threadNum) (Set.toList ctx)

getEqualities :: S.MonadState (FreestS a) m => m Equalities
getEqualities = S.gets equalities

-- addEquality :: S.MonadState (FreestS a) m => Span -> R.Equality -> m ()
-- addEquality span equality = S.modify (\s -> s { equalities = Set.insert (span, equality) (equalities s) })

addEquality :: S.MonadState (FreestS a) m => Span -> R.Equality -> String -> Int -> m ()
addEquality span equality function threadNum = do
  let (x,y) = equality
  xi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar x)
  yi <- getUnwrappedPriorityInstantiation $ extern (R.getLevelVar y)
  S.modify (\s -> s { equalities = Set.insert (R.EqualityEntry span equality function threadNum xi yi) (equalities s) })

addEquality' :: S.MonadState (FreestS a) m => Span -> R.Equality -> String -> Int -> Int -> m ()
addEquality' span equality function threadNum instantiation = S.modify (\s -> s { equalities = Set.insert (R.EqualityEntry span equality function threadNum instantiation (-1)) (equalities s) })

addDoubleVarEquality' :: S.MonadState (FreestS a) m => Span -> R.Equality -> String -> Int -> Int -> m ()
addDoubleVarEquality' span equality function threadNum instantiation = S.modify (\s -> s { equalities = Set.insert (R.EqualityEntry span equality function threadNum instantiation 0) (equalities s) })

-- getContextStack :: S.MonadState (FreestS a) m => m [T.Level]
-- getContextStack = S.gets context

-- getContextStack' :: S.MonadState (FreestS a) m => m [ContextSet]
-- getContextStack' = S.gets context'

getContextStack' :: S.MonadState (FreestS a) m => m [ContextSet]
getContextStack' = do
  stack <- S.gets context'
  return $ map (Set.map fst) stack

-- getContext :: S.MonadState (FreestS a) m => m T.Level
-- getContext = do
--   ctx <- S.gets context
--   case ctx of
--     (x:_) -> return x
--     []      -> return T.Top

-- getContext' :: S.MonadState (FreestS a) m => m ContextSet
-- getContext' = do
--   ctx <- S.gets context'
--   case ctx of
--     (x:_) -> return x
--     []      -> return Set.empty

getContext' :: S.MonadState (FreestS a) m => m ContextSet
getContext' = do
  ctx <- S.gets context'
  case ctx of
    (x:_) -> return $ Set.map fst x
    []    -> return Set.empty

getFullContext' :: S.MonadState (FreestS a) m => m InstantiatedContextSet
getFullContext' = do
  ctx <- S.gets context'
  case ctx of
    (x:_) -> return x
    []    -> return Set.empty

getGlobalContext' :: S.MonadState (FreestS a) m => m ContextSet
getGlobalContext' = do
  gctx <- S.gets globalContext'
  ctx <- getContext'
  ctxStack <- getContextStack'
  if gctx == Set.empty && length ctxStack == 1
    then do
      S.modify (\s -> s { globalContext' = ctx })
      return ctx
    else return gctx

resetGlobalContext' :: S.MonadState (FreestS a) m => m ()
resetGlobalContext' = do
  S.modify (\s -> s { globalContext' = Set.empty })
  S.modify (\s -> s { firstInContext' = T.Top })
  -- S.modify (\s -> s { latestInContext = T.Top })

-- updateContext' :: S.MonadState (FreestS a) m => T.Level -> m ()
-- updateContext' l = do
--   ctxStack <- getContextStack'
--   case ctxStack of
--     (x:xs) -> do
--       let newTop = Set.insert l x
--       S.modify (\s -> s { context' = newTop : xs })
--       if x == Set.empty 
--         then do
--           S.modify (\s -> s { firstInContext' = if firstInContext' s == T.Top then l else firstInContext' s })
--           S.modify (\s -> s { latestInContext = l })
--         else do
--           S.modify (\s -> s { firstInContext' = firstInContext' s })
--           S.modify (\s -> s { latestInContext = l })
--       -- S.modify (\s -> s { firstInContext = if firstInContext s == T.Top then l else firstInContext s })
--     [] -> do
--       gctx <- getGlobalContext'
--       if gctx == Set.empty
--         then do
--           S.modify (\s -> s { globalContext' = Set.singleton l })
--           S.modify (\s -> s { firstInContext' = if firstInContext' s == T.Top then l else firstInContext' s })
--           S.modify (\s -> s { latestInContext = l })
--           pushContext' l
--         else pushContext' l

updateContext' :: S.MonadState (FreestS a) m => T.Level -> m ()
updateContext' l = do
  ctxStack <- S.gets context'
  inst <- getUnwrappedPriorityInstantiation (show l)
  -- insts <- getPriorityInstantiations
  -- let inst = Map.findWithDefault 0 (show l) insts
  case ctxStack of
    (x:xs) -> do
      let newTop = Set.insert (l, inst) x
      S.modify (\s -> s { context' = newTop : xs })
      if x == Set.empty 
        then do
          S.modify (\s -> s { firstInContext' = if firstInContext' s == T.Top then l else firstInContext' s })
          S.modify (\s -> s { latestInContext = l })
        else do
          S.modify (\s -> s { firstInContext' = firstInContext' s })
          S.modify (\s -> s { latestInContext = l })
    [] -> do
      gctx <- getGlobalContext'
      if gctx == Set.empty
        then do
          S.modify (\s -> s { globalContext' = Set.singleton l })
          S.modify (\s -> s { firstInContext' = if firstInContext' s == T.Top then l else firstInContext' s })
          S.modify (\s -> s { latestInContext = l })
          pushContext' l
        else pushContext' l

newContext' :: S.MonadState (FreestS a) m => m ()
newContext' = S.modify (\s -> s { context' = Set.empty : context' s })

-- pushContext' :: S.MonadState (FreestS a) m => T.Level -> m ()
-- pushContext' l = S.modify (\s -> s { context' = Set.singleton l : context' s })

pushContext' :: S.MonadState (FreestS a) m => T.Level -> m ()
pushContext' l = do
  -- insts <- getPriorityInstantiations
  -- let inst = Map.findWithDefault (-1) (show l) insts
  inst <- getUnwrappedPriorityInstantiation (show l)
  S.modify (\s -> s { context' = Set.singleton (l, inst) : context' s })

-- popContext' :: S.MonadState (FreestS a) m => m ()
-- popContext' = do
--   ctxStack <- getContextStack'
--   case ctxStack of
--     (x:xs) -> do
--       gctx <- getGlobalContext'
--       S.modify (\s -> s { globalContext' = Set.union x gctx })
--       S.modify (\s -> s { context' = xs })
--     [] -> do
--       -- S.modify (\s -> s { globalContext' = Set.empty })
--       S.modify (\s -> s { context' = [] })

popContext' :: S.MonadState (FreestS a) m => m ()
popContext' = do
  ctxStack <- S.gets context'
  case ctxStack of
    (x:xs) -> do
      gctx <- S.gets globalContext'
      S.modify (\s -> s { globalContext' = Set.union (Set.map fst x) gctx })
      S.modify (\s -> s { context' = xs })
    [] -> S.modify (\s -> s { context' = [] })

popFirstInContext :: S.MonadState (FreestS a) m => m T.Level
popFirstInContext = do
  fic <- S.gets firstInContext'
  S.modify (\s -> s { firstInContext' = T.Top })
  return fic

getFirstInContext :: S.MonadState (FreestS a) m => m T.Level
getFirstInContext = S.gets firstInContext'

checkRenamedContext :: S.MonadState (FreestS a) m => T.Level -> m T.Level
checkRenamedContext l = do
  pc <- S.gets polyContext
  if pc /= T.Top
    then return pc
    else return l

setFirstInContext :: S.MonadState (FreestS a) m => T.Level -> m ()
setFirstInContext l = do
  fic <- S.gets firstInContext'
  -- if fic == T.Top
  --   then S.modify (\s -> s { firstInContext' = l })
  --   else return ()
  S.when (fic == T.Top) $ S.modify (\ s -> s {firstInContext' = l})

setPolyContext :: S.MonadState (FreestS a) m => T.Level -> m ()
setPolyContext l = do
  pc <- S.gets polyContext
  -- if pc == T.Top
  --   then S.modify (\s -> s { polyContext = l })
  --   else return ()
  S.when (pc == T.Top) $ S.modify (\ s -> s {polyContext = l})

clearFirstInContext :: S.MonadState (FreestS a) m => m ()
clearFirstInContext = do
  S.modify (\s -> s { firstInContext' = T.Top })
  S.modify (\s -> s { polyContext = T.Top })

getLatestInContext :: S.MonadState (FreestS a) m => m T.Level
getLatestInContext = S.gets latestInContext

clearLatestInContext :: S.MonadState (FreestS a) m => m ()
clearLatestInContext = S.modify (\s -> s { latestInContext = T.Top })

getLevelVarCounter :: S.MonadState (FreestS a) m => m Int
getLevelVarCounter = S.gets levelVarCounter

incrementLevelVarCounter :: S.MonadState (FreestS a) m => m ()
incrementLevelVarCounter = do
  n <- S.gets levelVarCounter
  S.modify (\s -> s { levelVarCounter = n + 1 })

minLevel' :: S.MonadState (FreestS a) m => Span -> [T.Level] -> m T.Level
minLevel' span ls = do
  let (isEdgeVal, l') = checkMinTopBot ls
  if isEdgeVal
    then return l'
    else do
      n <- S.gets levelVarCounter
      -- let newLevel = T.Num n
      let newLevel = T.LVar $ mkVar defaultSpan ("levelVar" ++ show n)
      incrementLevelVarCounter
      mapM_ (\l -> addInequality span (newLevel, l)) ls
      return newLevel
  -- n <- S.gets levelVarCounter
  -- let newLevel = T.Num n
  -- incrementLevelVarCounter
  -- -- S.modify (\s -> s { levelVarCounter = n + 1 })
  -- mapM_ (\l -> addInequality span (newLevel, l)) ls
  -- return newLevel

maxLevel' :: S.MonadState (FreestS a) m => Span -> [T.Level] -> m T.Level
maxLevel' span ls = do
  let (isEdgeVal, l') = checkMaxTopBot ls
  if isEdgeVal
    then return l'
    else do
      if all (\l -> R.compareLevels l (head ls)) ls
        then return (head ls)
        else do
          n <- S.gets levelVarCounter
          -- let newLevel = T.Num n
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

-- topBotMinLevel :: [T.Level] -> T.Level
-- topBotMinLevel [] = T.Top
-- topBotMinLevel [x] = x


-- maxLevel' ::  S.MonadState (FreestS a) m => Span -> [T.Level] -> m T.Level
-- maxLevel' span ls = do
--   n <- S.gets levelVarCounter
--   let newLevel = T.Num n
--   S.modify (\s -> s { levelVarCounter = n + 1 })
--   mapM_ (\l -> addInequality span (l, newLevel)) ls
--   return newLevel

-- levelOfTypeMap :: Span -> T.TypeMap -> m T.Level
-- levelOfTypeMap span tm
--   | Map.null tm = T.Top
--   -- | otherwise = foldr R.minLevel T.Top (map l (Map.elems tm))
--   | otherwise = minLevel' span (map l (Map.elems tm))
--   where
--     l (T.Labelled _ T.Record _ m)  = levelOfTypeMap span m
--     l (T.Labelled _ T.Variant _ m) = levelOfTypeMap span m
--     l t                            = R.level t

levelOfTypeMap :: S.MonadState (FreestS a) m => Span -> T.TypeMap -> m T.Level
levelOfTypeMap span tm
  | Map.null tm = return T.Top
  | otherwise = do
      ls <- mapM l (Map.elems tm)
      -- ls <- mapM getTypeLevel (Map.elems tm)
      minLevel' span ls
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

-- typeMapLevel :: Span -> T.TypeMap -> T.Level
-- typeMapLevel span tm
--   | Map.null tm = T.Top
--   | otherwise = 

registerFunctionPositions :: (S.MonadState (FreestS a) m, Show (XDef a)) => Definitions a -> m ()
registerFunctionPositions defs = do
  S.forM_ (Map.toList defs) $ \(k, v) -> do
    let span = getSpan k
    let (startingPos, _) = startPos span
    S.when (moduleName span /= "Prelude" && moduleName span /= "<default>") $ do
      S.modify (\s -> s { functionPositions = Map.insert (extern k) (FunctionData (startingPos, -1) [] 0 0) (functionPositions s) })
  orderFunctionPositions
  updateFunctionParams defs

getFunctionPositions :: S.MonadState (FreestS a) m => m (Map.Map String FunctionData)
getFunctionPositions = S.gets functionPositions

isFunctionRegistered :: S.MonadState (FreestS a) m => String -> m Bool
isFunctionRegistered name = do
  m <- S.gets functionPositions
  return $ Map.member name m

getCurrentFunction :: S.MonadState (FreestS a) m => Int -> m String
getCurrentFunction pos = do
  fps <- getFunctionPositions
  let res = find
        (\(_, FunctionData (start, end) _ _ _) ->
            start <= pos && (end == -1 || end >= pos))
        (Map.toList fps)
  return $ maybe "null" fst res

orderFunctionPositions :: S.MonadState (FreestS a) m => m ()
orderFunctionPositions = do
  m <- S.gets functionPositions
  let xs = sortOn (\(_, FunctionData (start, _) _ _ _) -> start) (Map.toList m)
      go [] = []
      go [(name, FunctionData (start, _) params i callNum)] = [(name, FunctionData (start, -1) params i callNum)]
      go ((name, FunctionData (start, _) params i callNum) : rest@((_, FunctionData (nextStart, _) _ _ _):_)) =
        (name, FunctionData (start, nextStart - 1) params i callNum) : go rest
      newMap = Map.fromList (go xs)
  S.modify (\s -> s { functionPositions = newMap })

isInFunction :: S.MonadState (FreestS a) m => String -> Span -> m Bool
isInFunction name span = do
  m <- getFunctionPositions
  return $ case Map.lookup name m of
    Just (FunctionData (start, end) _ _ _) -> 
      let (pos, _) = startPos span
      in pos >= start && lesserThan pos end
    Nothing -> False
    where
      lesserThan n1 n2 = n2 == -1 || n1 < n2

updateFunctionParams :: (S.MonadState (FreestS a) m, Show (XDef a)) => Definitions a -> m ()
updateFunctionParams defs = do
  fps <- getFunctionPositions
  let updateParams name fd =
        case [ v | (k, v) <- Map.toList defs, extern k == name ] of
          (v:_) ->
            let params = extractVars (show v)
            in fd { funcParams = params, funcParamIndex = length params - 1 }
          [] -> fd
      newMap = Map.mapWithKey updateParams fps
  S.modify (\s -> s { functionPositions = newMap })
  where
    extractVars s =
      [ takeWhile isAlphaNum (dropWhile (== '\\') w)
      | w <- words s
      , "\\" `isPrefixOf` w
      , ':' `elem` w
      ]

-- this function needs to be rewritten, we got repeated code
duplicateConstraintsInFunc :: S.MonadState (FreestS a) m => String -> Int -> m ()
duplicateConstraintsInFunc func ver = do
  ineqs <- getInequalities
  if ver > 0
    then do
      S.forM_ (Set.toList ineqs) $ \(R.InequalityEntry p (l1,l2) f n xi yi) -> do
        inFunc <- isInFunction func p
        if inFunc
          then do
            -- l1' <- renameLVar l1 ver
            -- l2' <- renameLVar l2 ver
            -- addInequality p (l1, l2) f n
            case (l1, l2) of
              (T.LNum _, _) -> return ()
              (_, T.LNum _) -> return ()
              _             -> addFullInequality p (l1, l2) func (ver + 1)
          else return ()
    else do
      S.forM_ (Set.toList ineqs) $ \(R.InequalityEntry p (l1,l2) f n xi yi) -> do
        inFunc <- isInFunction func p
        if inFunc
          then do
            -- l1' <- bindLVarToFunc l1 func
            -- l2' <- bindLVarToFunc l2 func
            S.modify (\s -> s { inequalities = Set.delete (R.InequalityEntry p (l1, l2) f n xi yi) (inequalities s) })
            addFullInequality p (l1, l2) func (ver + 1)
          else return ()

duplicateConstraintsInFunc' :: S.MonadState (FreestS a) m => String -> m ()
duplicateConstraintsInFunc' func = do
  call <- getFunctionCallsOf' func
  ineqs <- getInequalities
  S.forM_ (Set.toList ineqs) $ \(R.InequalityEntry p (l1,l2) f n xi yi) -> do
    inFunc <- isInFunction func p
    S.when inFunc $ do
      S.when (n == 0) $ S.modify (\s -> s { inequalities = Set.delete (R.InequalityEntry p (l1, l2) f n xi yi) (inequalities s) })
      addFullInequality' p (l1, l2) func call xi yi

pushLevelToAbstractionContext :: S.MonadState (FreestS a) m => T.Level -> m ()
pushLevelToAbstractionContext l = S.modify (\s -> s { abstractionContext = l : abstractionContext s })

getAbstractionContext :: S.MonadState (FreestS a) m => m [T.Level]
getAbstractionContext = S.gets abstractionContext

popLevelFromAbstractionContext :: S.MonadState (FreestS a) m => m T.Level
popLevelFromAbstractionContext = S.state $ \s -> case abstractionContext s of
  []     -> (T.Top, s)
  (x:xs) -> (x, s { abstractionContext = xs, abstractionStack = x : abstractionStack s })

clearAbstractionContext :: S.MonadState (FreestS a) m => m ()
clearAbstractionContext = S.modify (\s -> s { abstractionContext = [] })

substituteAbstractionContext :: S.MonadState (FreestS a) m => Variable -> T.Level -> m ()
substituteAbstractionContext v l = S.modify $ \s ->
  s { abstractionContext = map (substLevel v l) (abstractionContext s) }
  where
    substLevel v l (T.LVar v')
      | v == v'   = l
      | otherwise = T.LVar v'
    substLevel v l (T.LAdd l1 l2) = T.LAdd (substLevel v l l1) (substLevel v l l2)
    substLevel _ _ l =  l

getAbstractionStack :: S.MonadState (FreestS a) m => m [T.Level]
getAbstractionStack = S.gets abstractionStack

clearAbstractionStack :: S.MonadState (FreestS a) m => m ()
clearAbstractionStack = S.modify (\s -> s { abstractionStack = [] })

-- addEndpointPriority :: S.MonadState (FreestS a) m => Variable -> (Int, Int) -> m ()
-- addEndpointPriority v p = S.modify (\s -> s { endpointPriorities = Map.insert v p (endpointPriorities s) })

-- addEndpointPriority :: S.MonadState (FreestS a) m => Variable -> String -> (Int, Int) -> m ()
-- addEndpointPriority v func (x,y) = do
--   S.modify (\s -> s { endpointPriorities = Map.insert (v, func) (x,y) (endpointPriorities s) })

addEndpointPriority :: S.MonadState (FreestS a) m => Variable -> String -> (Int, Int) -> m ()
addEndpointPriority v func (x, y) = do
  fps <- getFunctionPositions
  let callNum = maybe 0 functionCallNum (Map.lookup func fps)
  S.modify (\s -> s { endpointPriorities = Map.insert (v, func, callNum) (x, y) (endpointPriorities s) })

-- addEndpointPriorities :: S.MonadState (FreestS a) m => (Variable, Variable) -> (Int, Int) -> m ()
-- addEndpointPriorities (v1, v2) p = do
--   addEndpointPriority v1 p
--   addEndpointPriority v2 p

addEndpointPriorities :: S.MonadState (FreestS a) m => (Variable, Variable) -> String -> (Int, Int) -> m ()
addEndpointPriorities (v1, v2) func p = do
  addEndpointPriority v1 func p
  addEndpointPriority v2 func p

getEndpointPriorities :: S.MonadState (FreestS a) m => m EndpointPriorities
getEndpointPriorities = S.gets endpointPriorities

-- getEndpointPriority :: S.MonadState (FreestS a) m => Variable -> m (Maybe (Int, Int))
-- getEndpointPriority v = do Map.lookup v <$> getEndpointPriorities

-- getEndpointPriority :: S.MonadState (FreestS a) m => Variable -> String -> m (Maybe (Int, Int))
-- getEndpointPriority v func = do
--   Map.lookup (v, func) <$> getEndpointPriorities

getEndpointPriority :: S.MonadState (FreestS a) m => Variable -> String -> m (Maybe (Int, Int))
getEndpointPriority v func = do
  fps <- getFunctionPositions
  let callNum =  maybe 0 functionCallNum (Map.lookup func fps)
  Map.lookup (v, func, callNum) <$> getEndpointPriorities

getEndpointPriorityByName :: S.MonadState (FreestS a) m => String -> String -> m (Maybe (Int, Int))
getEndpointPriorityByName varName funcName = do
  fps <- getFunctionPositions
  let callNum = maybe 0 functionCallNum (Map.lookup funcName fps)
  mp <- getEndpointPriorities
  let match = [ epd
              | ((v, f, c), epd) <- Map.toList mp
              , extern v == varName
              , f == funcName
              , c == callNum
              ]
  return $ listToMaybe match

updateLatestFreshEndpoints :: S.MonadState (FreestS a) m => (Variable, Variable) -> m ()
updateLatestFreshEndpoints ep = S.modify (\s -> s { latestFreshEndpoints = ep })

getLatestFreshEndpoints :: S.MonadState (FreestS a) m => m (Variable, Variable)
getLatestFreshEndpoints = S.gets latestFreshEndpoints

getFunctionParam :: S.MonadState (FreestS a) m => String -> m (Maybe String)
getFunctionParam func = do
  fps <- getFunctionPositions
  case Map.lookup func fps of
    Just fd@(FunctionData pos params paramIndex callNum) ->
      if paramIndex >= 0 && paramIndex < length params
        then do
          let param = params !! paramIndex
              newIndex = if paramIndex == 0 then length params - 1 else paramIndex - 1
              newFd = fd { funcParamIndex = newIndex }
          S.modify (\s -> s { functionPositions = Map.insert func newFd fps })
          return $ Just param
        else return Nothing
    Nothing -> return Nothing

addFunctionCall' :: S.MonadState (FreestS a) m => String -> Int -> m ()
addFunctionCall' func line = do
  multi <- isMultipleArgFunctionCall func line
  S.unless multi $ do
    fps <- getFunctionPositions
    case Map.lookup func fps of
      Just fd@(FunctionData pos params paramIndex callNum) -> do
        let newFd = fd { functionCallNum = callNum + 1 }
        S.modify (\s -> s { functionPositions = Map.insert func newFd fps })
        S.modify (\s -> s { calledFunctions = Map.insert func line (calledFunctions s) })
      Nothing -> return ()
  where
    isMultipleArgFunctionCall :: S.MonadState (FreestS a) m => String -> Int -> m Bool
    isMultipleArgFunctionCall func line = do
      called <- S.gets calledFunctions
      return $ case Map.lookup func called of
        Just l  -> l == line
        Nothing -> False

getFunctionCallsOf' :: S.MonadState (FreestS a) m => String -> m Int
getFunctionCallsOf' func = do
  fps <- getFunctionPositions
  case Map.lookup func fps of
    Just fd -> return $ functionCallNum fd
    Nothing -> return 0

-- isMultipleArgFunctionCall :: S.MonadState (FreestS a) m => String -> Int -> m Bool
-- isMultipleArgFunctionCall func line = do
--   called <- S.gets calledFunctions
--   return $ case Map.lookup func called of
--     Just l -> l == line
--     Nothing -> False

addPriorityInstantiation :: S.MonadState (FreestS a) m => String -> m ()
addPriorityInstantiation var = do
  m <- S.gets priorityInstantiations
  let newMap = case Map.lookup var m of
        Nothing -> Map.insert var 0 m
        Just v  -> Map.insert var (v + 1) m
  S.modify (\s -> s { priorityInstantiations = newMap })

getPriorityInstantiations :: S.MonadState (FreestS a) m => m (Map.Map String Int)
getPriorityInstantiations = S.gets priorityInstantiations

getPriorityInstantiation :: S.MonadState (FreestS a) m => String -> m (Maybe Int)
getPriorityInstantiation var = do
  let var' = isolateVarNum var
  m <- S.gets priorityInstantiations
  return $ Map.lookup var' m

getUnwrappedPriorityInstantiation :: S.MonadState (FreestS a) m => String -> m Int
getUnwrappedPriorityInstantiation var = do
  let var' = isolateVarNum var
  m <- S.gets priorityInstantiations
  return $ Map.findWithDefault 0 var' m

clearPriorityInstantiations :: S.MonadState (FreestS a) m => m ()
clearPriorityInstantiations = S.modify $ \s ->
  let merged = Map.union (globalPriorityInstantiations s) (priorityInstantiations s)
  in s { globalPriorityInstantiations = merged, priorityInstantiations = Map.empty }

getGlobalPriorityInstantiations :: S.MonadState (FreestS a) m => m (Map.Map String Int)
getGlobalPriorityInstantiations = S.gets globalPriorityInstantiations

isolateVarNum :: String -> String
isolateVarNum s = case filter isValid (splitPlus s) of
  []    -> s
  ws    -> last ws
  where
    splitPlus :: String -> [String]
    splitPlus = words . map (\c -> if c == '+' then ' ' else c)
    isValid w = not (all isDigit w) && w /= "+" && not (null w)

getFirstInequalitySpan :: S.MonadState (FreestS a) m => m (Maybe Span)
getFirstInequalitySpan = do
  ineqs <- S.gets inequalities
  return $ case Set.toList ineqs of
    (R.InequalityEntry span _ _ _ _ _ : _) -> Just span
    [] -> Nothing