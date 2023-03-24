{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns,MultiWayIf #-}
module Elaboration.KindInference where


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
import           Elaboration.Elaboration
import           Elaboration.InfState
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



cg :: K.KindEnv -> T.Type -> InferState K.Kind
cg _ (T.Int s) = return $ K.ut s
cg _ (T.Char s) = return $ K.ut s
cg _ (T.String s) = return $ K.ut s
cg _ (T.Labelled s T.Record m) | Map.null m = return $ K.ut s -- Unit
cg kEnv (T.Var _ x) = case kEnv Map.!? x of
  Just k -> return k
  Nothing -> error $ "Var " ++ show x ++ " not in scope " -- TODO: Proper error  
cg _ (T.Skip s) = return  $ K.us s
cg _ (T.End s) = return $ K.ls s
cg kEnv (T.Message s _ t) = cg kEnv t $> K.ls s
cg kEnv (T.Labelled s (T.Choice _) m) = tMapM (cg kEnv) m $> K.ls s
cg kEnv (T.Semi s t u) = do
  k1 <- cg kEnv t
  k2 <- cg kEnv u
  addKConstraint (k1, K.ls s)
  addKConstraint (k2, K.ls s)
  m <- freshMultVar
  addMConstraint (m, [k1,k2])
  return $ K.ls s
cg kEnv (T.Rec p (Bind _ a k@(K.Kind _ K.Un K.Session) t@(T.Semi _ T.Message{} (T.Var _ b))))
  | a == b  = cg (Map.insert a k kEnv) t $> K.us p
cg _ (T.Rec p (Bind _ a (K.Kind _ K.Un K.Session) (T.Labelled _ (T.Choice _) m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ K.us p
cg kEnv (T.Rec _ b) = do --  (Bind K.Kind Type)
  k <- cg (Map.insert (var b) (binder b) kEnv) (body b)
  addKConstraint (k, binder b)
  addKVariableFromK (binder b)
  return (binder b)
cg kEnv (T.Arrow s m t u) = do
  cg kEnv t
  cg kEnv u
  return $ K.Kind s (convMult m) K.Top
cg kEnv (T.Labelled s _ m) = do
  ks <- foldM (\acc t -> cg kEnv t >>= \k -> return $ k : acc) [] m
  fmv <- freshMultVar
  addMConstraint (fmv, ks)
  let k = K.Kind s fmv K.Top
  mapM_ (\k' -> addKConstraint (k',k)) ks  -- TODO: new
  return k
cg kEnv t@(T.Forall s b) = do
  k <- cg (Map.insert (var b) (binder b) kEnv) (body b)
--  traceM ("isForall "++ show t)
  fmv <- freshMultVar
  addMConstraint (fmv, [k])
  addKVariableFromK (binder b)
  return $ K.Kind s fmv K.Top
cg kEnv d@(T.Dualof s t) = do
  k <- cg kEnv t
  fmv <- freshMultVar
  addMConstraint (fmv, [k])
  addKConstraint (k, K.ls s)
--  traceM $ ">>>> " ++ show d ++ " "++ show k ++ " " ++ show fmv
  return $ K.Kind s fmv K.Session

-- TODO: find proper place
convMult :: Multiplicity -> K.Multiplicity
convMult Un = K.Un
convMult Lin = K.Lin


-- Expressions



type UsageSet = Set.Set (Variable, K.Kind)

weaken :: E.Exp -> Variable -> K.Kind -> UsageSet -> InferState ()
weaken e x k sigma
--  | (x, k) `elem` sigma = return ()
  | memberOnFst sigma x = return ()
  | otherwise = do
      -- traceM $ "\nWeak: Adding constraint " ++ show (getSpan e) ++ " " ++ show x ++ " " ++ show k ++ " " ++ show e ++"\n"
      addKConstraint (k, K.Kind defaultSpan K.Un K.Top)
      -- fmv <- freshMultVar      
      -- addMConstraint (fmv, [k])
      -- addMConstraint (fmv, [K.ut defaultSpan])

join :: UsageSet -> InferState ()
join = void . setTraversal (\(_,k) -> -- traceM ("Join " ++ show k) >>
                             addKConstraint (k, K.Kind defaultSpan K.Un K.Top))
        -- freshMultVar >>=
        --         \fmv ->
                  -- addMConstraint (fmv, [k])
                  --    >> addMConstraint (fmv, [K.ut defaultSpan]))
       
setTraversal :: (Ord a, Monad m) => (b -> m a) -> Set.Set b -> m (Set.Set a)
setTraversal f s = Set.fromList <$> mapM f (Set.toList s) 


infGen :: K.KindEnv -> E.Exp -> InferState (T.Type, UsageSet)

infGen _ (E.Int s _) = return (T.Int s, Set.empty)
infGen _ (E.String s _) = return (T.String s, Set.empty)
infGen _ (E.Char s _) = return (T.Char s, Set.empty)
infGen kEnv (E.Unit s) = let t = T.unit s in 
  cg kEnv t $> (t, Set.empty) 
infGen kEnv (E.App p fork@(E.Var _ x) e) | x == mkFork p = do
  s <- get
  let (T.Arrow _ _ _ t2, u1) = evalState (infGen kEnv e) s
  put s
  (t3, u2) <- infGen kEnv (E.App p (E.TypeApp p fork t2) e)
  
--  join (u1 ∩ u2) 
  return (t3, u2) -- u1 ∪ u2)
  -- Select C e
infGen kEnv (E.App p (E.App _ (E.Var _ x) (E.Var _ c)) e) | x == mkSelect p = do
  (t, u) <- infGen kEnv e
  varEnv <- getVarEnv
  let m = evalState (Extract.inChoiceMap e t) (initialState{varEnv})
  let t1 = evalState (Extract.choiceBranch p m c t) (initialState{varEnv})
  return (t1, u)


infGen kEnv (E.App _ (E.Var p x) e) | x == mkCollect p = do
  (t,u) <- infGen kEnv e
  varEnv <- getVarEnv
  let (tm,s) = runState (Extract.outChoiceMap e t) (initialState{varEnv})
  when (hasErrors s) (error $ show $ getErrors s)
--  traceM $ ">>> " ++ show t ++ " ----> " ++ show tm
  
  return (T.Labelled p T.Variant -- (T.Choice T.External)
    (Map.map (T.Labelled p T.Record . Map.singleton (head mkTupleLabels p)) tm), u)

infGen kEnv (E.App p (E.Var _ x) e) | x == mkReceive p = do
  (t,u) <- infGen kEnv e
  varEnv <- getVarEnv
  let (t1, t2) = evalState (Extract.input e t) (initialState{varEnv})
  return (T.tuple p [t1, t2], u)

  -- Send e1 e2
infGen kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkSend p = do
  (t1, u1) <- infGen kEnv e1
  (t2, u2) <- infGen kEnv e2
  varEnv <- getVarEnv
  let (t3, t4) = evalState (Extract.output e2 t2) (initialState{varEnv})
  -- cg kEnv t3
  -- cg kEnv t4
  join (u1 ∩ u2) 
  return (t4,u1 ∪ u2)
--   -- Close e1
infGen kEnv (E.App p (E.Var _ x) e) | x == mkClose p = do
  (t',u) <- infGen kEnv e
  varEnv <- getVarEnv
  let _ = evalState (Extract.end e t')  (initialState{varEnv})
  -- TODO: if cannot extract error (join)
  return (T.unit p, u)

infGen kEnv e@(E.App s e1 e2) = do
  (t,u1) <- infGen kEnv e1
--  traceM $ show t
  varEnv <- getVarEnv
  let (m, t1, t2) = evalState (Extract.function e1 t) (initialState{varEnv})
  (_, u2) <- infGen kEnv e2
  cg kEnv (T.Arrow s m t1 t2)
  
  -- traceM $ "---------------\n"
  --   ++ show e ++ "\n"
  --   ++ show u1 ++ "\n"
  --   ++ show u2 ++ "\n"
  --   ++ show (u1 ∩ u2) ++ "\n---------------"
  
  join (u1 ∩ u2)
  return (t2, u1 ∪ u2)
  
infGen kEnv (E.Var p x) = do
  vEnv <- getVarEnv
  if Map.member x vEnv then do
    let t = vEnv Map.! x
    k <- cg kEnv t
    return (t, Set.singleton (x, k))
  else
     traceM (show x ++ ":" ++ show (getSpan x) ++  " is not in Delta") $>
     (T.Int defaultSpan, Set.empty)
infGen kEnv e@(E.Abs s m b) = do
  k <- cg kEnv (binder b)
  addToVarEnv (var b) (binder b)
  (t, u) <- infGen kEnv (body b)
  weaken e (var b) k u
--  traceM $ "var: " ++ show (var b) ++ "\nbinder: " ++ show (body b) ++ "\n" ++ show t ++ "\n-----------------\n"
  when (isClosure $ body b) ( do
--    let (T.Arrow _ m' _ _) = t in
    case t of
      (T.Arrow _ m' _ _) -> addKConstraint (k, K.Kind s (convMult m') K.Top) -- >>
        -- traceM ("\nisClosure for var " ++ show (var b) ++ " : " ++ show k
        --      ++ " <: " ++ show (K.Kind s (convMult m') K.Top) ++ "\n"
        --     ++ show t ++ "\n")
      f@(T.Forall _ (Bind _ _ _ (T.Arrow _ m' _ _)))     ->
        addKConstraint (k, K.Kind s (convMult m') K.Top)
             --    traceM ("\nisClosure for var " ++ show (var b) ++ " : " ++ show k
             -- ++ " <: " ++ show (K.Kind s (convMult m') K.Top) ++ "\n"
             -- ++ show t ++ "\n")

      t -> error $ show t
      
    -- traceM ("\nisClosure for var " ++ show (var b) ++ " : " ++ show k
    --          ++ " <: " ++ show (K.Kind s (convMult m') K.Top) ++ "\n"
    --          ++ show t ++ "\n")
    -- addKConstraint (k, K.Kind s (convMult m') K.Top)
    )
  rmFromVarEnv (var b)  
  return (T.Arrow s m (binder b) t, (var b,k) `Set.delete` u)
  where
    isClosure E.Abs{} = True
    isClosure (E.TypeAbs _ b) = isClosure $ body b
    isClosure _ = False

    
-- different the fresh variable comes from the parser
infGen kEnv (E.TypeAbs s b) = do
  let kEnv' = Map.insert (var b) (binder b) kEnv
  (t, u) <- infGen kEnv' (body b)

  -- traceM ("\n" ++ show (var b) ++ " : " ++ show (binder b) ++
  --         "\n" ++ show (body b) ++
  --         "\n" ++ show t ++
  --         "\n")
  
--  cg kEnv' t
--  addKConstraint (binder b, K.lt s)
  addKVariableFromK (binder b)
  return (T.Forall s (Bind s (var b) (binder b) t), u)

-- infGen kEnv (E.TypeApp p new@(E.Var _ x) t) | x == mkNew p = do
--   u                             <- synthetise kEnv new
--   ~(T.Forall _ (Bind _ y k u')) <- Extract.forall new u
--   -- TODO: is there a better way of doing this for `new`?
--   -- check against a new 'Endable' kind?
--   void $ K.checkAgainst kEnv k t
--   return $ Rename.subs t y u'



infGen kEnv tapp@(E.TypeApp _ e t) = do
  k <- cg kEnv t
  (t1, u) <- infGen kEnv e
  varEnv <- getVarEnv
  let (T.Forall _ b) = evalState (Extract.forall e t1) (initialState{varEnv})
  -- cg (Map.insert (var b) (binder b) kEnv) (body b)
--  addKConstraint (k, k1) --binder b)
  
  addKConstraint (k, binder b)
  -- traceM $ "typeApp: " ++ show tapp ++ " : " ++ show k ++ "\n" ++ show (binder b) ++ "\n"
  return (subs t (var b) (body b), u)

infGen kEnv e@(E.BinLet s x y e1 e2) = do
  (t1, u1) <- infGen kEnv e1
--  traceM $ "Binlet:  " ++ show e ++ "  " ++ show t1 ++ "\n"
  varEnv <- getVarEnv
  let (t2, t3) = evalState (Extract.pair e1 t1) (initialState{varEnv})
  addToVarEnv x t2 
  addToVarEnv y t3
  (t, u2) <- infGen kEnv e2
--  traceM $ "~~~~~~~~> " ++ show t
  _ <- cg kEnv t
  k1 <- cg kEnv t2
  k2 <- cg kEnv t3
  join (u1 ∩ u2)
  let varKind = [(x,k1), (y,k2)] -- zip [x,y] (Map.elems ks)
  mapM_ (\(v, k) -> weaken e v k u2) varKind
  let u = foldr Set.delete (u1 ∪ u2) varKind
-- --  let u = map (`Set.delete` u') 
  return (t, u)
  
  
infGen kEnv e@(E.UnLet s x e1 e2) = do
  (t, u1) <- infGen kEnv e1
  addToVarEnv x t
  (u, u2) <- infGen kEnv e2
  k <- cg kEnv t
  -- _ <- cg kEnv u
  
  -- traceM $ "\nunlet: "
  --   ++ show e ++ "\n"
  --   ++ show k ++ "\n"
  --   ++ show t ++ "\n"
  --   ++ show u1 ++ "\n"
  --   ++ show u2 ++ "\n"
    
  join (u1 ∩ u2)
  weaken e x k (u1 ∪ u2) 
  return (u, Set.delete (x, k) (u1 ∪ u2))

infGen kEnv (E.Case s e m) = do
--  ~(T.Labelled s1 T.Variant m1, u1) <- infGen kEnv e
  (t, u1) <- infGen kEnv e
  varEnv <- getVarEnv
  let dta = evalState (Extract.datatypeMap e t) (initialState{varEnv})
  let (m1,sta) = runState (buildMap s m dta) (initialState{varEnv})

  -- traceM $ "\n--------------------\nCASE:\nm =  " ++ show m         
  --        ++ "\n\nm1 = " ++ show m1
  --        ++ "\n\nt =  " ++ show t
  --        ++ "\n\ndta = " ++ show dta
  --        ++ "\n--------------------\n"
--  when (hasErrors sta) (error $ show (errors sta))
  -- (tl, ul) <- Map.foldr (\(xs, e') acc -> acc >>= \(ts, us) ->
  --               infGen kEnv e' >>= \(t',u') -> return (t':ts,u':us)) (return ([],[])) m1
  (tl, ul) <- Map.foldr (infGenMap kEnv) (return ([],[])) m1

  -- mapM_ (cg kEnv) tl
  -- let union = foldl Set.union u1 ul
  -- foldM_ (\acc u -> join (acc ∩ u) $> acc ∪ u) Set.empty ul
--  traceM $ show $ head tl
  return (head tl, foldr (flip Set.union) u1 ul)

  
infGen kEnv e@(E.Pair s e1 e2) = do
  (t, u1) <- infGen kEnv e1
  (u, u2) <- infGen kEnv e2 
  -- cg kEnv t
  -- cg kEnv u
  let (l0:l1:_) = mkTupleLabels -- head mkTupleLabels defaultSpan
  let m = Map.insert (l0 defaultSpan) t (Map.singleton (l1 defaultSpan) u)
  join (u1 ∩ u2)
  return (T.Labelled s T.Record m, u1 ∪ u2)
  
infGen _ e = error $ "undefined: " ++ show e 


infGenMap :: K.KindEnv -> ([Variable], E.Exp) ->
             InferState ([T.Type], [UsageSet]) -> InferState ([T.Type], [UsageSet])
infGenMap kEnv (xs, e) state = do
  (ts, us) <- state
  (t,u) <- infGen kEnv e
  return (returnType xs t : ts, u:us)
  where
    returnType :: [Variable] -> T.Type -> T.Type
    returnType [] t                  = t
    returnType (_:xs) (T.Arrow _ _ _ t2) = returnType xs t2
    returnType _ t = t

(∪) :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
(∪) = Set.union

(∩) :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b) -> Set.Set (a,b)
(∩) s1 s2 = x -- Set.filter (memberOnFst s1 . fst) s2
             `Set.union`
            y -- Set.filter (memberOnFst s2 . fst) s1
  where
    x = Set.foldl (\acc (k,v) -> if memberOnFst s2 k then Set.insert (k,v) acc else acc) (getDups s1) s1
    y = Set.foldl (\acc (k,v) -> if memberOnFst s1 k then Set.insert (k,v) acc else acc) (getDups s2) s2

    getDups :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b)
    getDups = snd . Set.foldl (\(acc1,acc2) (k,v) ->  if Set.member k acc1
                          then (acc1, Set.insert (k,v) acc2)
                          else (Set.insert k acc1, acc2)) (Set.empty, Set.empty)
              
  -- Set.foldl (\(acc1, acc2) v -> if memberOnFst v acc2
  --                       then (Set.insert v acc1,  )
  --                       else ()  ) (Set.empty, s1) s2
--   Set.intersection

memberOnFst :: Ord a => Set.Set (a,b) -> a -> Bool
memberOnFst s v = Set.member v $ Set.map fst s













------------------------------------------------------------
-- RESOLVE CONSTRAINTS
------------------------------------------------------------


type SubsK = Map.Map Variable K.Kind
type SubsM = Map.Map Variable K.Multiplicity

topKind = K.lt defaultSpan
topMult = K.Lin

infer :: Variable -> InferState (SubsK, SubsM)
infer path = do
  -- kc <- foldl cleanConstK [] <$> getKConstraints
  -- mc <- foldl cleanConstM [] <$> getMConstraints
  kc <- getKConstraints
  mc <- getMConstraints

  -- kc' <- getKConstraints
  -- mc' <- getMConstraints
  -- let kc = foldl cleanConstK [] kc'
  -- let mc = foldl cleanConstM [] mc'
  -- traceM ("funname " ++ show path ++
  --         "\nKinds: " ++ show (length kc) ++ "\n" ++ show (length kc) ++
  --         "\nMults: " ++ show (length mc) ++
  --         "\n----------\n")
  
  let joinCtxs = map Left kc ++ map Right mc
  subsK <- initialSubs topKind getKVariables
  subsM <- initialSubs topMult getMVariables
  
  inferAll (subsK, subsM) joinCtxs
  -- (sk,sm) <- inferAll (subsK, subsM) joinCtxs
  -- return (Map.foldlWithKey (\acc mv m -> Map.map (subsOnK mv m) acc) sk sm, sm)
 where
   initialSubs :: a -> InferState (Set.Set Variable) -> InferState (Map.Map Variable a)
   initialSubs top getVar = Set.foldl (\acc k -> Map.insert k top acc) Map.empty <$> getVar

cleanConstK :: [KindConstraint] -> KindConstraint -> [KindConstraint]
cleanConstK acc (K.Kind _ K.Lin K.Session, K.Kind _ K.Un K.Top) = acc
cleanConstK acc ks@(k1,k2)
  | properKind k1 && properKind k2 =
      if k1 SK.<: k2 then acc else
        traceM ("Constraint, proper error " ++ show k1 ++ " <: " ++ show k2) >> ks:acc
  | otherwise = ks:acc
    
properKind :: K.Kind -> Bool
properKind (K.KindVar _ _) = False
properKind (K.Kind _ K.MultVar{} _) = False
properKind _ = True

cleanConstM :: [MultConstraint] -> MultConstraint -> [MultConstraint]
cleanConstM acc (_,ks) | all (\case {(K.Kind _ m _) -> m == K.Lin; _ -> False}) ks = acc
cleanConstM acc ks = ks:acc


inferAll :: (SubsK, SubsM) -> [Either KindConstraint MultConstraint] -> InferState (SubsK, SubsM)
inferAll s cs = do
  res@(sk,_) <- inferAll' s cs
--  let errs = Map.foldlWithKey (\acc x k -> if ) [] c
  let erros = foldl (\acc e -> case e of
                        Right _ -> acc
                        Left (k1,k2)
                          | isKVar k1 && isKVar k2 -> cmp (sk Map.! fromKToVar k1) (sk Map.! fromKToVar k2) acc
                          | isKVar k1 -> cmp (sk Map.! fromKToVar k1) k2 acc                              
                          | isKVar k2 -> cmp k1 (sk Map.! fromKToVar k2) acc
                          | otherwise -> acc
                    ) [] cs
  unless (null erros) (error $ show erros)
  return res
  where
    cmp k1 k2 acc
      | k1 SK.<: k2 = acc
      | otherwise   = (show k1 ++ " <: " ++ show k2) : acc
  
inferAll' :: (SubsK, SubsM) -> [Either KindConstraint MultConstraint] -> InferState (SubsK, SubsM)
inferAll' s cs = do
  !s' <- foldM inferOne s cs
  if s == s' then pure s' else inferAll' s' cs


inferOne :: (SubsK, SubsM) -> Either KindConstraint MultConstraint -> InferState (SubsK, SubsM)
-- inferOne (sk, sm) (Left (K.Kind _ K.Lin K.Session, K.Kind _ K.Un K.Top)) = return (sk,sm)
-- inferOne (sk, sm) (Left (K.Kind _ (K.MultVar m) p1, K.Kind _ K.Lin p2)) = return (sk,sm)
-- inferOne (sk, sm) (Left (K.Kind _ (K.MultVar m) p1, K.Kind _ K.Un p2))
--   | p1 SK.<: p2 = return (sk, Map.insert m K.Un sm) 
--   | otherwise = error "Complete me"
inferOne (sk, sm) (Left (k1,k2))
  | isKVar k1 = do
      let k1' = fromKToVar k1
      let k2' = getFromSubs k2
      let k3 = sk Map.! k1'
      return (Map.insert k1' (SK.meet k2' k3) sk, sm)
  | isKVar k2 =  do -- TODO: just continue & in the end verify
      -- let k1' = getFromSubs k1
      -- let k2' = sk Map.! fromKToVar k2
      -- if k1' SK.<: k2' then
        return (sk, sm)
--      else error ("Contraint infraction1: " ++ show k1'  ++ " is not a subkind (<:) of " ++ show k2') -- $> (sk,sm)
  | otherwise = do
      if k1 SK.<: k2
        then return (sk, sm)
        else error ("Contraint infraction2: " ++ show k1 ++ " is not a subkind (<:) of " ++ show k2) -- $> (sk,sm)       
 where
   getFromSubs (K.KindVar _ k) = sk Map.! k
   getFromSubs (K.Kind s (K.MultVar m) p) = K.Kind s (sm Map.! m) p
   getFromSubs k = k
  
inferOne (sk, sm) (Right (mv,ks)) =
  let lub = foldl upperBound K.Un (map getMults ks) in
  return (sk, Map.insert (fromMToVar mv) lub sm)
  where
    getMults k
      | isKVar k  = mult $ sk Map.! fromKToVar k
      | otherwise = mult k
      
    upperBound currLeast m
      | isMVar m  = SK.join (sm Map.! fromMToVar m) currLeast
      | otherwise = SK.join m currLeast














-- inferOne :: (SubsK, SubsM) -> Either KindConstraint MultConstraint -> InferState (SubsK, SubsM)
-- inferOne (sk, sm) (Left (K.Kind _ K.Lin K.Session, K.Kind _ K.Un K.Top)) = return (sk,sm)
-- inferOne (sk, sm) (Left (K.Kind _ (K.MultVar m) p1, K.Kind _ K.Lin p2)) = return (sk,sm)
-- inferOne (sk, sm) (Left (K.Kind _ (K.MultVar m) p1, K.Kind _ K.Un p2))
--   | p1 SK.<: p2 = return (sk, Map.insert m K.Un sm) 
--   | otherwise = error "Complete me"
-- inferOne (sk, sm) (Left (k1,k2))
--   | isKVar k1 = do
--       let k1' = fromKToVar k1
--       let k2' = getFromSubs k2
--       if k2' SK.<: (sk Map.! k1')
--         then return (Map.insert k1' k2' sk, sm)
--         else return (sk,sm)
--   | isKVar k2 =  do
--       let k1' = getFromSubs k1
--       let k2' = fromKToVar k2
--       let k3 = sk Map.! k2'
--       if | isKVar k3 -> return (sk,sm)
--          | k1' SK.<: k3 -> return (sk, sm)
--          | otherwise    -> error ("Contraint infraction1: " ++ show k1'
--                                ++ " is not a subkind (<:) of " ++ show k3) -- $> (sk,sm)
--       -- else if k1' SK.<: k3 -- (sk Map.! k2')

--       --   then return (sk, sm)
--       --   else 
--   | otherwise = do
--       if k1 SK.<: k2
--         then return (sk, sm)
--         else error ("Contraint infraction2: " ++ show k1
--                   ++ " is not a subkind (<:) of " ++ show k2) -- $> (sk,sm)       
--  where
--    getFromSubs (K.KindVar _ k) = sk Map.! k
--    getFromSubs (K.Kind s (K.MultVar m) p) = K.Kind s (sm Map.! m) p
--    getFromSubs k = k
-- inferOne (sk, sm) (Right (mv,ks)) =
--   let lub = foldl upperBound K.Un (map getMults ks) in
--   return (sk, Map.insert (fromMToVar mv) lub sm)
--   where
--     getMults k
--       | isKVar k  = mult $ sk Map.! fromKToVar k
--       | otherwise = mult k
      
--     upperBound currLeast m
--       | isMVar m  = SK.join (sm Map.! fromMToVar m) currLeast
--       | otherwise = SK.join m currLeast


