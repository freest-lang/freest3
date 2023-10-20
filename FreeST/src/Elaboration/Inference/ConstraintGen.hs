{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Elaboration.Inference.ConstraintGen where


import           Control.Monad.State.Lazy hiding (join)
import           Data.Bifunctor as Bifunctor
import           Data.Functor
import           Data.List
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import qualified Data.Traversable as Traversable
import           Debug.Trace
-- import           Elaboration.Elaboration
-- import           Elaboration.InfState
import           Parse.ParseUtils
import           Parse.Parser ( parseProgram, parseAndImport )
import           Parse.Read
import           Paths_FreeST ( getDataFileName )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import           Syntax.Program
import qualified Syntax.Type as T
import           System.IO
import           Util.FreestState -- (tMapM_,tMapM, initialState, FreestS(..), RunOpts(..))
import qualified Validation.Extract as Extract
import           Validation.Rename -- hiding (subs)
import qualified Validation.Subkind as SK
-- import           Validation.Substitution
import           Validation.Typing
import Validation.Kinding (typeToKindMult, unr, checkContractive)


-- type InferState = State InferS

-- type KindConstraint = (K.Kind, K.Kind)
-- type MultConstraint = (K.Multiplicity, [K.Kind]) 

-- type KindConstraints = [KindConstraint]
-- type MultConstraints = [MultConstraint]


-- data InferS = InferS { index ::  Int
--                      , kConstraints :: KindConstraints
--                      , mConstraints :: MultConstraints
--                      -- , kVariables :: Set.Set Variable
--                      -- , mVariables :: Set.Set Variable
--                      -- , vEnv       :: VarEnv
--                      , ers :: Errors
--                      } deriving Show

-- initial :: InferS
-- initial = InferS { index  = 0
--                  , kConstraints = []
--                  , mConstraints = []
--                  , ers = []
--                  -- , kVariables   = Set.empty
--                  -- , mVariables   = Set.empty
--                  -- , vEnv         = Map.empty
--                  -- , bindVarKind  = Map.empty
--                  }

-- addKConstraint :: KindConstraint -> InferState ()
-- addKConstraint c = modify (\s -> s { kConstraints = c : kConstraints s })

-- addMConstraint :: MultConstraint -> InferState ()
-- addMConstraint c = modify (\s -> s { mConstraints = c : mConstraints s })

-- getIndex :: InferState Int
-- getIndex = do
--   next <- gets index
--   modify (\s -> s { index = next + 1 })
--   return next

-- freshKindVar :: Span -> InferState K.Kind
-- freshKindVar s = do
--   i <- getIndex
--   return $ K.KindVar s $ mkVar defaultSpan ("χ" ++ show i )

-- freshMultVar :: InferState K.Multiplicity
-- freshMultVar = do
--   i <- getIndex
--   let v = mkVar defaultSpan ("φ" ++ show i )
-- --  addMVariable v
--   return $ K.MultVar v

-- -- addKVariableFromK :: K.Kind -> InferState ()
-- -- addKVariableFromK (K.KindVar _ v) = addKVariable v
-- -- addKVariableFromK _             = return ()

-- -- addKVariable :: Variable -> InferState ()
-- -- addKVariable c = modify (\s -> s { kVariables = c `Set.insert` kVariables s })



-- | Constraint Generation from types
cg :: K.KindEnv -> T.Type -> FreestState K.Kind
cg kEnv = cg' (Map.keysSet kEnv) kEnv

cg' :: K.PolyVars -> K.KindEnv -> T.Type -> FreestState K.Kind
cg' _ _ (T.Int s) = return $ K.ut s
cg' _ _ (T.Char s) = return $ K.ut s
cg' _ _ (T.String s) = return $ K.ut s
cg' _ _ (T.Labelled s T.Record m) | Map.null m = return $ K.ut s -- Unit
cg' _ kEnv (T.Var _ x) = case kEnv Map.!? x of
  Just k -> return k
  Nothing -> error $ "Var " ++ show x ++ " not in scope " -- TODO: Proper error  
cg' _ _ (T.Skip s) = return  $ K.us s
cg' _ _ (T.End s) = return $ K.la s
cg' pvs kEnv (T.Message s _ t) = cg' pvs kEnv t $> K.ls s
cg' pvs kEnv (T.Labelled s (T.Choice _) m) = -- do
  -- ks <- tMapM (cg' pvs kEnv) m
  -- return $ Map.foldr (\(K.Kind _ m1 v1) (K.Kind p m2 v2) -> K.Kind p (SK.join m1 m2) (SK.meet v1 v2))
  --   (snd $ Map.elemAt 0 ks) ks  
  tMapM (cg' pvs kEnv) m $> K.ls s
cg' pvs kEnv (T.Semi s t u) = do
  k1 <- cg' pvs kEnv t
  k2 <- cg' pvs kEnv u
  m <- freshMultVar
  pk <- freshPreKindVar
  addKConstraint (k1, K.ls s)
  addKConstraint (k2, K.ls s)
  addMConstraint (m, [k1,k2])
  addPKConstraint (pk, (k1,k2))
  return $ K.Kind s m pk

  -- k1 <- cg' pvs kEnv t
  -- k2 <- cg' pvs kEnv u
  -- addKConstraint (k1, K.ls s)
  -- addKConstraint (k2, K.ls s)
  -- m <- freshMultVar
  -- addMConstraint (m, [k1,k2])
  -- return $ K.ls s
cg' pvs kEnv (T.Rec p (Bind _ a k@(K.Kind _ K.Un K.Session) t@(T.Semi _ T.Message{} (T.Var _ b))))
  | a == b  = cg' pvs (Map.insert a k kEnv) t $> K.ua p
cg' pvs _ (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Labelled _ (T.Choice _) m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ K.ua p
cg' pvs kEnv (T.Rec _ b) = do --  (Bind K.Kind Type)
  checkContractive pvs (var b) (body b)
  
  addKVariableFromK (binder b) 
  let kEnv' = Map.insert (var b) (binder b) kEnv 

  k <- cg' pvs kEnv' (body b)
  addKConstraint (k, binder b)
  addKConstraint (binder b, k)
  
  if unr (Map.keysSet kEnv' Set.\\ pvs) (body b) then do
--    debugM $ show (body b)
    fm <- freshMultVar
    addMConstraint (fm, [k])
    let rk = K.Kind (getSpan $ binder b) fm K.Absorb
    addKConstraint (binder b, rk) $> rk
  else pure k
  
--   return (binder b)
cg' pvs kEnv (T.Arrow s m t u) = do
  cg' pvs kEnv t
  cg' pvs kEnv u
  return $ K.Kind s (typeToKindMult m) K.Top

  
cg' pvs kEnv l@(T.Labelled s _ m) = do
  ks <- foldM (\acc t -> cg' pvs kEnv t >>= \k -> return $ k : acc) [] m
  fmv <- freshMultVar
--  debugM $ ">>> " ++ show ks 
  addMConstraint (fmv, foldl (\acc k -> if isKVar k then acc else k:acc ) [] ks)
  let k = K.Kind s fmv K.Top
  -- mapM_ (\k' -> addKConstraint (k',k)) ks  -- TODO: new
  return k
  where
    isKVar :: K.Kind -> Bool
    isKVar K.KindVar{} = True
    isKVar _           = False


  
cg' pvs kEnv t@(T.Forall s b) = do
  addKVariableFromK (binder b)  
  k <- cg' (Set.insert (var b) pvs) (Map.insert (var b) (binder b) kEnv) (body b)
  fmv <- freshMultVar
  addMConstraint (fmv, [k])
  return $ K.Kind s fmv K.Top
cg' pvs kEnv d@(T.Dualof s t) = do
  k <- cg' pvs kEnv t
  fmv <- freshMultVar
  addMConstraint (fmv, [k])
  addKConstraint (k, K.ls s)
  return $ K.Kind s fmv K.Session


------------------------------------------------------------

-- | Constraint Generation from expressions




type UsageSet = Set.Set (Variable, K.Kind)

weaken :: E.Exp -> Variable -> K.Kind -> UsageSet -> FreestState ()
weaken e x k sigma
--  | (x, k) `elem` sigma = return ()
  | memberFst sigma x = return ()
  | otherwise = addKConstraint (k, K.Kind defaultSpan K.Un K.Top)

join :: E.Exp -> UsageSet -> FreestState ()
join e = void . setTraversal (\(_,k) -> do
    --                           when (prekinds k) (debugM $ "addKConstraint " ++ show e ++ " : " ++ show k ++ " <: " ++ show (K.ut defaultSpan))
                               addKConstraint (k, K.Kind defaultSpan K.Un K.Top))


prekinds (K.Kind _ _ v) = v == K.Session
prekinds _ = False
       
setTraversal :: (Ord a, Monad m) => (b -> m a) -> Set.Set b -> m (Set.Set a)
setTraversal f s = Set.fromList <$> mapM f (Set.toList s) 

memberFst :: Ord a => Set.Set (a,b) -> a -> Bool
memberFst s v = Set.member v $ Set.map fst s

(∪) :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
(∪) = Set.union

(∩) :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b) -> Set.Set (a,b)
(∩) s1 s2 = x -- Set.filter (memberOnFst s1 . fst) s2
             `Set.union`
            y -- Set.filter (memberOnFst s2 . fst) s1
  where
    x = Set.foldl (\acc (k,v) -> if memberFst s2 k then Set.insert (k,v) acc else acc) (getDups s1) s1
    y = Set.foldl (\acc (k,v) -> if memberFst s1 k then Set.insert (k,v) acc else acc) (getDups s2) s2

    getDups :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b)
    getDups = snd . Set.foldl (\(acc1,acc2) (k,v) ->  if Set.member k acc1
                          then (acc1, Set.insert (k,v) acc2)
                          else (Set.insert k acc1, acc2)) (Set.empty, Set.empty)
         

infGen :: K.KindEnv -> E.Exp -> FreestState (T.Type, UsageSet)
infGen _ (E.Int s _) = return (T.Int s, Set.empty)
infGen _ (E.String s _) = return (T.String s, Set.empty)
infGen _ (E.Char s _) = return (T.Char s, Set.empty)
infGen kEnv (E.Unit s) = {- let t = T.unit s in cg kEnv t $> -} return (T.unit s, Set.empty) 
infGen kEnv (E.App p fork@(E.Var _ x) e) | x == mkFork p = do
  s <- get
  (_, _, t2) <- Extract.function e (fst $ evalState (infGen kEnv e) s)
  put s
  infGen kEnv (E.App p (E.TypeApp p fork t2) e)
infGen kEnv (E.App p (E.App _ (E.Var _ x) (E.Var _ c)) e) | x == mkSelect p = do
  (t, u) <- infGen kEnv e
  m <- Extract.inChoiceMap e t
  t1 <- Extract.choiceBranch p m c t
  return (t1, u)
infGen kEnv (E.App _ (E.Var p x) e) | x == mkCollect p = do
  (t,u) <- infGen kEnv e
  tm <- Extract.outChoiceMap e t  
  return (T.Labelled p T.Variant
    (Map.map (T.Labelled p T.Record . Map.singleton (head mkTupleLabels p)) tm), u)
infGen kEnv (E.App p (E.Var _ x) e) | x == mkReceive p = do
  (t,u) <- infGen kEnv e
  (t1, t2) <- Extract.input e t
  return (T.tuple p [t1, t2], u)
infGen kEnv e@(E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkSend p = do
  (t1, u1) <- infGen kEnv e1
  (t2, u2) <- infGen kEnv e2
  (t3, t4) <- Extract.output e2 t2
  join e (u1 ∩ u2) 
  return (t4, u1 ∪ u2)
infGen kEnv (E.App p (E.Var _ x) e) | x == mkClose p = do
  (t',u) <- infGen kEnv e
  Extract.end e t'
  return (T.unit p, u)
infGen kEnv e@(E.App s e1 e2) = do
  (t,u1) <- infGen kEnv e1
  (m, t1, t2) <- Extract.function e1 t
  (_, u2) <- infGen kEnv e2
--  cg kEnv (T.Arrow s m t1 t2)
  join e (u1 ∩ u2)
  return (t2, u1 ∪ u2)
infGen kEnv (E.Var p x) = getFromVEnv x >>= \case
  Just t -> do
    k <- cg kEnv t
    return (t, Set.singleton (x, k))
  Nothing -> -- traceM (show x ++ ":" ++ show (getSpan x) ++  " is not in Delta") $>
    -- TODO: ERROR
    addToVEnv x (omission defaultSpan) $>
    (T.Int defaultSpan, Set.empty)
infGen kEnv e@(E.Abs s m b) = do
  k <- cg kEnv (binder b)
  addToVEnv (var b) (binder b)
  (t, u) <- infGen kEnv (body b)
  weaken e (var b) k u
  wh3
    case t of
      (T.Arrow _ m' _ _) -> addKConstraint (k, K.Kind s (typeToKindMult m') K.Top)
      f@(T.Forall _ (Bind _ _ _ (T.Arrow _ m' _ _)))     ->
        addKConstraint (k, K.Kind s (typeToKindMult m') K.Top)
      t -> error $ show t
    )
  removeFromVEnv (var b)  
  return (T.Arrow s m (binder b) t, (var b,k) `Set.delete` u)
  where
    isClosure E.Abs{} = True
    isClosure (E.TypeAbs _ b) = isClosure $ body b
    isClosure _ = False
-- different the fresh variable comes from the parser
infGen kEnv (E.TypeAbs s b) = do
  addKVariableFromK (binder b)
  (t, u) <- infGen (Map.insert (var b) (binder b) kEnv) (body b)
  return (T.Forall s (Bind s (var b) (binder b) t), u)
infGen kEnv tapp@(E.TypeApp _ e t) = do
  k <- cg kEnv t
  (t1, u) <- infGen kEnv e
  ~(T.Forall _ b) <- Extract.forall e t1
  addKConstraint (k, binder b)
  return (subs t (var b) (body b), u)
infGen kEnv e@(E.BinLet s x y e1 e2) = do
  (t1, u1) <- infGen kEnv e1
  (t2, t3) <- Extract.pair e1 t1
  addToVEnv x t2 
  addToVEnv y t3
  (t, u2) <- infGen kEnv e2
--  _ <- cg kEnv t
  k1 <- cg kEnv t2
  k2 <- cg kEnv t3
  join e (u1 ∩ u2)
  let varKind = [(x,k1), (y,k2)] -- zip [x,y] (Map.elems ks)
  mapM_ (\(v, k) -> weaken e v k u2) varKind
  return (t, foldr Set.delete (u1 ∪ u2) varKind)
infGen kEnv e@(E.UnLet s x e1 e2) = do
  (t, u1) <- infGen kEnv e1
  addToVEnv x t
  (u, u2) <- infGen kEnv e2
  k <- cg kEnv t
  -- _ <- cg kEnv u
  join e (u1 ∩ u2)
  weaken e x k (u1 ∪ u2) 
  return (u, Set.delete (x, k) (u1 ∪ u2))
infGen kEnv (E.Case s e m) = do
  (t, u1) <- infGen kEnv e
  dta <- Extract.datatypeMap e t
  m1 <- buildMap s m dta
  (tl, ul) <- Map.foldr (infGenMap kEnv) (return ([],[])) m1
  return (head tl, foldr (flip Set.union) u1 ul)
infGen kEnv e@(E.Pair s e1 e2) = do
  (t, u1) <- infGen kEnv e1
  (u, u2) <- infGen kEnv e2 
  -- cg kEnv t
  -- cg kEnv u
  let (l0:l1:_) = mkTupleLabels -- head mkTupleLabels defaultSpan
  let m = Map.insert (l0 defaultSpan) t (Map.singleton (l1 defaultSpan) u)
  join e (u1 ∩ u2)
  return (T.Labelled s T.Record m, u1 ∪ u2)  
infGen _ e = error $ "undefined: " ++ show e 


infGenMap :: K.KindEnv -> ([Variable], E.Exp) ->
             FreestState ([T.Type], [UsageSet]) -> FreestState ([T.Type], [UsageSet])
infGenMap kEnv (xs, e) state = do
  (ts, us) <- state
  (t,u) <- infGen kEnv e
  return (returnType xs t : ts, u:us)
  where
    returnType :: [Variable] -> T.Type -> T.Type
    returnType [] t                  = t
    returnType (_:xs) (T.Arrow _ _ _ t2) = returnType xs t2
    returnType _ t = t
