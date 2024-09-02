{-# LANGUAGE FlexibleContexts #-}
{-|

Module      :  Typing.Typing
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase #-}

module Typing.Typing
  -- ( typeCheck
  -- , synthetise -- for tests
  -- , checkAgainst -- for tests
  -- , checkDefs
  -- , buildMap
  -- )
where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import qualified Syntax.Type as T
import           Syntax.Value
import           Equivalence.TypeEquivalence (equivalent)
import           Equivalence.Subtyping (subtype)
import qualified Typing.Extract as Extract
import qualified Typing.Rename as Rename ( subs )
import           Typing.Phase hiding (Typing)
import qualified Kinding.Kinding as K
import           Util.Error
import           Util.State hiding (void)
import           Util.Warning
import           Parse.Unparser () -- debug

import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.State ( evalState, MonadState (get), evalStateT, liftIO)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           System.Timeout (timeout)

import Debug.Trace
import qualified Data.Set as Set
import qualified Typing.Substitution as TSubs
import Data.Char
import Data.Maybe
import           Typing.Normalisation ( normalise )


typeCheck :: TypingState ()
typeCheck = do
  s0 <- get
  setErrors []
  -- * Check the formation of all type decls
  mapM_ (uncurry $ K.checkAgainst Map.empty) =<< getTypes
  -- * Check the formation of all function signatures
  mapM_ (K.synthetise Map.empty) =<< getSignatures
  -- Gets the state and only continues if there are no errors so far  
  s <- get
  unless (hasErrors s) $ do
    -- * Remove signatures with definitions
    defs <- getDefs
    sigs <- getSignatures
    setSignatures $ Map.filterWithKey (\k _ -> Map.notMember k defs) sigs
    -- * Check definitions in evaluation order
    mapM_ (checkDefs sigs) =<< getEvalOrder
    -- * Check the main function
    checkMainFunction
    -- * Checking final environment for linearity
    checkLinearity
    
  -- Get the state again to join the error messages
  -- here, we continue with the errors from the previous state (kind inference) 
  s <- get
  setErrors (errors s ++ errors s0)
  

checkDefs :: Signatures -> [Variable] -> TypingState () 
checkDefs sigs [ ] = return () 
checkDefs sigs xs = do 
  let xts = map (\x -> (x, sigs Map.! x)) xs
  mapM_ (uncurry addToSignatures) xts 
  mapM_ (checkDef xs) xts 
  where 
    checkDef :: [Variable] -> (Variable, T.Type) -> TypingState ()
    checkDef xs (x,t) = do
      defs <- getDefs 
      case defs Map.!? x of  
        Nothing -> return () 
        Just e  -> do 
          when (length xs > 1 && not (isVal e)) 
            (addError (MutualDefNotValue (getSpan x) x e))
          k <-  K.synthetise Map.empty t 
          when (K.isLin k && Map.member x sigs) $ removeFromSignatures x
          checkAgainst Map.empty e t      
          when (Map.member x sigs) $ addToSignatures x t

-- Check a given function body against its type; make sure all linear
-- variables are used. Deprecated (see function checkDefs above).
checkFunBody :: Signatures -> Variable -> E.Exp -> TypingState ()
checkFunBody sigs f e = forM_ (sigs Map.!? f) checkBody
  where
    checkBody t = do
      sigs <- getSignatures
      k <-  K.synthetise Map.empty t 
      when (K.isLin k && Map.member f sigs) (removeFromSignatures f)
      checkAgainst Map.empty e t      
      when (Map.member f sigs) $ addToSignatures f t

checkMainFunction :: TypingState ()
checkMainFunction = do
  runOpts <- getRunOpts
  sigs <- getSignatures
  let main = getMain runOpts
  if main `Map.member` sigs
    then do
      let t = sigs Map.! main
      k <- K.synthetise Map.empty t
      when (K.isLin k) $
        let sp = getSpan $ fst $ Map.elemAt (Map.findIndex main sigs) sigs in
        addError (UnrestrictedMainFun sp main t k)
    else when (isMainFlagSet runOpts) $
      addError (MainNotDefined (defaultSpan {moduleName = runFilePath runOpts}) main)

checkLinearity :: TypingState ()
checkLinearity = do
  sigs <- getSignatures
  m <- filterM (K.lin . snd) (Map.toList sigs)
  unless (null m) $ addError (LinearFunctionNotConsumed (getSpan (fst $ head m)) m)


synthetise :: K.KindEnv -> E.Exp -> TypingState T.Type
-- Basic expressions
synthetise _ (E.Int  p _  ) = return $ T.Int p
synthetise _ (E.Float p _ ) = return $ T.Float p
synthetise _ (E.Char p _  ) = return $ T.Char p
synthetise _ (E.Unit p    ) = return $ T.unit p
synthetise _ (E.String p _) = return $ T.String p
synthetise kEnv e@(E.Var _ x) = synthApp "SYNTH" kEnv e Nothing >>
  getFromSignatures x >>= \case
    Just s -> do
      k <- K.synthetise kEnv s
      when (K.isLin k) $ removeFromSignatures x
      return s
    Nothing -> do
      let p = getSpan x
          s = omission p
      addError (VarOrConsNotInScope p x)
      addToSignatures x s
      return s
-- Unary let
synthetise kEnv (E.UnLet _ x e1 e2) = do
  t1 <- synthetise kEnv e1
  addToSignatures x t1
  t2 <- synthetise kEnv e2
  difference kEnv x
  return t2
-- Abstraction
synthetise kEnv e'@(E.Abs p mult (Bind _ x t1 e)) = do
  void $ K.synthetise kEnv t1
  sigs1 <- getSignatures -- Redundant when mult == Lin
  addToSignatures x t1
  t2 <- synthetise kEnv e
  difference kEnv x
  when (mult == Un) (do
    sigs2 <- getSignatures
    checkEquivEnvs (getSpan e) NonEquivEnvsInUnFun e' kEnv sigs1 sigs2)
  return $ T.Arrow p mult t1 t2
-- Application, the special cases first
  -- Select C e
synthetise kEnv (E.App p (E.App _ (E.Var _ x) (E.Var _ c)) e)
  | x == mkSelect p = do
    t <- synthetise kEnv e
    m <- Extract.inChoiceMap e t
    Extract.choiceBranch p m c t
  -- Collect e
synthetise kEnv (E.App _ (E.Var p x) e) | x == mkCollect p = do
  tm <- Extract.outChoiceMap e =<< synthetise kEnv e
  return $ T.Labelled p T.Variant
    (Map.map (T.Labelled p T.Record . Map.singleton (head mkTupleLabels p)) tm)
  -- Receive e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkReceive p = do
  t        <- synthetise kEnv e
  (u1, u2) <- Extract.input e t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  return $ T.tuple p [u1, u2]
  -- Send e1 e2
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkSend p = do
  t        <- synthetise kEnv e2
  (u1, u2) <- Extract.output e2 t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  checkAgainst kEnv e1 u1
  return u2
  -- fork e
-- synthetise kEnv (E.App p fork@(E.Var _ x) e) | x == mkFork p = do
--   s <- get
--   u <- liftIO $ evalStateT (synthetise kEnv e) s
--   (_, _, t) <- Extract.function e u
--   synthetise kEnv (E.App p (E.TypeApp p fork t) e)
-- Application, general case
synthetise kEnv e@(E.App _ e1 e2) = do
  synthApp "SYNTH" kEnv e Nothing
  -- t        <- synthetise kEnv e1
  -- (_, u1, u2) <- Extract.function e1 t
  
  -- -- checkAgainst kEnv e2 u1
  -- return u2
-- Type abstraction
synthetise kEnv e@(E.TypeAbs _ (Bind p a k e')) =
  unless (isVal e') (addError (TypeAbsBodyNotValue (getSpan e') e e')) >>
  T.Forall p . Bind p a k <$> synthetise (Map.insert a k kEnv) e'
-- New @t - check that t comes to an End
synthetise kEnv (E.TypeApp p new@(E.Var _ x) t) | x == mkNew p = do
  u                             <- synthetise kEnv new
  ~(T.Forall _ (Bind _ y _ u')) <- Extract.forall new u
  void $ K.checkAgainstAbsorb kEnv t
  return $ Rename.subs t y u'
-- Type application
synthetise kEnv (E.TypeApp _ e t) = do
  u                               <- synthetise kEnv e
  ~(T.Forall _ (Bind _ y k u')) <- Extract.forall e u
  void $ K.checkAgainst kEnv k t
  return $ Rename.subs t y u'
-- Pair introduction
synthetise kEnv (E.Pair p e1 e2) = do
  t1 <- synthetise kEnv e1
  t2 <- synthetise kEnv e2
  return $ T.Labelled p T.Record $
    Map.fromList (zipWith (\ml t -> (ml $ getSpan t, t)) mkTupleLabels [t1, t2])
-- Pair elimination
synthetise kEnv (E.BinLet _ x y e1 e2) = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToSignatures x u1
  addToSignatures y u2
  t2 <- synthetise kEnv e2
  difference kEnv x
  difference kEnv y
  return t2
-- Datatype elimination
synthetise kEnv (E.Case p e fm) = do
  fm'  <- buildMap p fm =<< Extract.datatypeMap e =<< synthetise kEnv e
  sigs <- getSignatures
  ~(t : ts, v : vs) <- Map.foldr (synthetiseMap kEnv sigs)
                                 (return ([], [])) fm'
  mapM_ (compareTypes e t) ts
  mapM_ (checkEquivEnvs p NonEquivEnvsInBranch e kEnv v) vs
  setSignatures v
  return t

synthetiseMap :: K.KindEnv -> Signatures -> ([Variable], E.Exp)
              -> TypingState ([T.Type], [Signatures])
              -> TypingState ([T.Type], [Signatures])
synthetiseMap kEnv sigs (xs, e) state = do
  (ts, envs) <- state
  t          <- synthetise kEnv e
  env        <- getSignatures
  setSignatures sigs
  return (returnType xs t : ts, env : envs)
 where
  returnType :: [Variable] -> T.Type -> T.Type
  returnType [] t                  = t
  returnType (_:xs) (T.Arrow _ _ _ t2) = returnType xs t2
  returnType _ t                  = t

-- The difference operation. Removes a program variable from the
-- variable environment and gives an error if it is linear
difference :: K.KindEnv -> Variable -> TypingState ()
difference kEnv x = do
  getFromSignatures x >>= \case
    Just t -> do
      k <- K.synthetise kEnv t
      when (K.isLin k) $ addError (LinProgVar (getSpan x) x t k)
    Nothing -> return ()
  removeFromSignatures x

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> TypingState ()

-- Pair elimination
checkAgainst kEnv (E.BinLet _ x y e1 e2) t2 = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToSignatures x u1
  addToSignatures y u2
  checkAgainst kEnv e2 t2
  difference kEnv x
  difference kEnv y
-- TODO Datatype elimination
-- checkAgainst kEnv (Case p e fm) = checkAgainstFieldMap p kEnv e fm Extract.datatypeMap
-- Abs elimination. It seems that we cannot do checkAgainst for we
-- cannot decide whether to use a Lin or a Un function. See
-- counterexamples: polySUTL.fst when using Lin, and mult.fst when
-- using Un
checkAgainst kEnv e@(E.App p e1 e2) t = do
  -- t <- synthetise kEnv e2
  -- checkAgainst kEnv e1 (Fun p Un/Lin t u)
    debugM $ "APP Check Against {\n"
        ++ "\t * e:    " ++ show e ++ "\n"
        ++ "\t * t:    " ++ show t ++ "\n"
        ++ "\t * kEnv: " ++ show kEnv ++ "\n"
        ++ "              }\n"
    void $ synthApp "CA" kEnv e (Just t)

checkAgainst  kEnv e@(E.Var _ x) t = do
--  void $ synthApp "CA" kEnv e (Just t)
  getFromSignatures x >>= \case
    Just s -> do
      k <- K.synthetise kEnv s
      when (K.isLin k) $ removeFromSignatures x
    Nothing -> return ()

-- synthetise kEnv e'@(E.Abs p mult (Bind _ x t1 e)) = do
--   void $ K.synthetise kEnv t1
--   sigs1 <- getSignatures -- Redundant when mult == Lin
--   addToSignatures x t1
--   t2 <- synthetise kEnv e
--   difference kEnv x
--   when (mult == Un) (do
--     sigs2 <- getSignatures
--     checkEquivEnvs (getSpan e) NonEquivEnvsInUnFun e' kEnv sigs1 sigs2)
--   return $ T.Arrow p mult t1 t2


checkAgainst  kEnv e'@(E.Abs p mult (Bind _ x t e)) u = do
  let (T.Arrow _ _ u1 u2) = normalise u

--  void $ K.synthetise kEnv t
  sigs1 <- getSignatures -- Redundant when mult == Lin
  addToSignatures x t
  
  compareTypes e' u1 t
  debugM $ "******** " ++ show u2
  checkAgainst kEnv e u2
  difference kEnv x
  when (mult == Un) (do
    sigs2 <- getSignatures
    checkEquivEnvs (getSpan e) NonEquivEnvsInUnFun e' kEnv sigs1 sigs2)

  return ()
  -- void $ synthApp "CA" kEnv e (Just t)
    
checkAgainst kEnv e'@(E.TypeAbs _ (Bind _ x k e)) t = do
  ~xx@(T.Forall _ (Bind s y k u')) <- Extract.forall e' t
  -- TODO: x vs y and also the k's??

  debugM $ "TABS checkAgainst {\n"
        ++ "\t* e: " ++ show e ++ "\n"
        ++ "\t* e': " ++ show e' ++ "\n"
        ++ "\t* t: " ++ show t ++ "\n"
        ++ "\t* extracted: " ++ show xx ++ "\n"
        ++ "}\n"
  -- [t/a]u, substitute t for for every occurrence of a in u


  checkAgainst (Map.insert x k kEnv) e (TSubs.subs (T.Var s x) y u')

  
checkAgainst kEnv e t = do
  debugM $ "Generic checkAgainst {\n"
        ++ "\t* e: " ++ show e ++ "\n"
        ++ "\t* t: " ++ show t ++ "\n"
        ++ "}\n"
  sub <- subtyping <$> getRunOpts
  case t of 
    t@(T.Arrow _ Lin t1 t2) | not sub -> do 
      (_, u1, u2) <- Extract.function e =<< synthetise kEnv e 
      compareTypes e u1 t1 
      compareTypes e u2 t2 
    _ -> compareTypes e t =<< synthetise kEnv e

compareTypes :: E.Exp -> T.Type -> T.Type -> TypingState () 
compareTypes e t u = do 
  sub <- subtyping <$> getRunOpts
  timeout_ms   <- subTimeout_ms <$> getRunOpts
  let cmp = if sub then subtype else equivalent 
  checkAttempt <- liftIO $ timeout (timeout_ms * 10^3) (evaluate $ cmp u t)
  case checkAttempt of 
    Just checks -> unless checks 
                 $ addError (TypeMismatch (getSpan e) t u e)
    Nothing     -> addError (TypeCheckTimeout (getSpan e) sub t u e timeout_ms)

checkEquivEnvs :: Span -> (Span -> Signatures -> Signatures -> E.Exp -> ErrorType) ->
                   E.Exp -> K.KindEnv -> Signatures -> Signatures -> TypingState ()
checkEquivEnvs p error exp _ sigs1 sigs2 =
  -- unless (equivalent kEnv sigs1 sigs2) $
  unless (Map.keysSet sigs1 == Map.keysSet sigs2) $
    addError (error p (sigs1 Map.\\ sigs2) (sigs2 Map.\\ sigs1) exp)

-- Build abstractions for each case element

buildMap :: MonadState (FreestS a) m => Span -> E.FieldMap -> T.TypeMap -> m E.FieldMap
buildMap p fm tm = do
  when (tmS /= fmS && tmS > fmS) $ addWarning (NonExhaustiveCase p fm tm)
  tMapWithKeyM (buildAbstraction tm) fm
  where tmS = Map.size tm
        fmS = Map.size fm


buildAbstraction :: MonadState (FreestS a) m => T.TypeMap -> Variable -> ([Variable], E.Exp)
                 -> m ([Variable], E.Exp)
buildAbstraction tm x (xs, e) = case tm Map.!? x of
  Just (T.Labelled _ T.Record rtm) -> let n = Map.size rtm in
    if n /= length xs
      then addError (WrongNumOfCons (getSpan e) x n xs e) $> (xs, e)
      else return (xs, buildAbstraction' (xs, e) (map snd $ Map.toList rtm))
  Just t -> internalError "variant not a record type" t
  Nothing -> -- Data constructor not in scope
    addError (DataConsNotInScope (getSpan x) x) $> (xs, e)
 where
  buildAbstraction' :: ([Variable], E.Exp) -> [T.Type] -> E.Exp
  buildAbstraction' ([], e) _ = e
  buildAbstraction' (x : xs, e) (t:ts) =
    E.Abs (getSpan e) Lin $ Bind (getSpan e) x t $ buildAbstraction' (xs, e) ts



{- ***********************
 *
 * Inference relative
 *
*************************-}




data Nary = Nary E.Exp [Nary]
--          | Simple E.Exp
          deriving Show

-- toNary function to convert Exp to Nary
toNary :: E.Exp -> Nary
toNary e@E.Var{} = Nary e []    -- Base case: A simple variable
toNary exp = uncurry Nary (collectApps exp [])
  where
    collectApps (E.App _ f x) acc = collectApps f (simpleOrNary x : acc)
    collectApps f acc         = (f, acc)

    simpleOrNary e@E.App{} = toNary e  -- Keep converting if it's an application
    simpleOrNary exp       = Nary exp []        -- Otherwise, it's a simple expression


fromNary :: Nary -> E.Exp
fromNary (Nary h xs) =
  foldl (\acc e -> case e of
            Nary h [] -> E.App (getSpan h) acc h
            nary@(Nary h _) -> E.App (getSpan h) acc (fromNary nary)
        ) h xs
-- fromNary (Simple e) = e

type Sigma = Map.Map Variable T.Type

freshInstVar :: Span -> TypingState Variable
freshInstVar s = do
  i <- getNextIndex
  return $ mkVar s (show i ++ "κ")

-- TODO: Env and add only binds 
fiv :: T.Type -> Set.Set Variable
fiv t@(T.Var _ x)
  | isFiv t = Set.singleton x
  | otherwise = Set.empty
fiv (T.Arrow _ _ t u) = fiv t `Set.union` fiv u
fiv (T.Forall _ b) = fiv (body b)
fiv t = Set.empty -- TODO: finish (wrong)


isFiv (T.Var _ x) = dropWhile isDigit (show x) == "κ"
isFiv _ = False

-- returns a type 𝜙𝑖 for each of the 𝜋; and the result type 𝜌
inst :: T.Type -> [Nary] -> TypingState ([T.Type], T.Type) 
inst t xs = baseInst t xs >>= \(_,xs,u) -> pure (xs, u)
  where
    baseInst :: T.Type -> [Nary] -> TypingState (Sigma, [T.Type], T.Type) 
    baseInst t [] = pure (Map.empty, [], t) -- IRESULT
    baseInst (T.Forall _ (Bind s y _ t)) xs = do -- IALL 
      k <- freshInstVar s
      baseInst (TSubs.subs (T.Var s k) y t) xs
    baseInst a@(T.Arrow _ _ t u) (x:xs) = do -- IArg
      sigma1 <- quicklook x t
      (sigma2, ts, ret) <- baseInst (fullSubs sigma1 u) xs
      let sigma = sigma1 `Map.union` sigma2


      
      debugM $ "baseInst " ++ " {\n"
        ++ "\t * x:xs: " ++ show (x:xs) ++ "\n"
        ++ "\t * type: " ++ show a ++ "\n"
        ++ "\t * rho: " ++ show (fullSubs sigma t) ++ "\n"
        ++ "\t * sigma1: " ++ show sigma1 ++ "\n"
        -- ++ "\t * sigma2: " ++ show sigma2 ++ "\n"
        -- ++ "\t * ts (u): " ++ show ts ++ "\n"
        -- ++ "\t * res (u): " ++ show ret ++ "\n"
        ++ "                              }\n"
      return (sigma, fullSubs sigma t : ts,ret)

    baseInst x@(T.Var s t) xs
      | isFiv x = do -- IVar
          k1 <- freshInstVar s
          k2 <- freshInstVar s
          let arrow = T.Arrow s Un (T.Var s k1) (T.Var s k2)
          (sigma, ts, ret) <- baseInst arrow xs
          return (Map.singleton t arrow `Map.union` sigma, ts,ret)
    baseInst t _ = pure (Map.empty, [], t)
  
--   undefined

-- TODO finish
fullSubs :: Sigma -> T.Type -> T.Type
fullSubs = fullSubs' Set.empty
  where
    fullSubs' :: Set.Set Variable -> Sigma -> T.Type -> T.Type
    fullSubs' s sigma t@(T.Var _ x) =
      case sigma Map.!? x of
        Just t -> if Set.member x s then t else fullSubs' (Set.insert x s) sigma t
        Nothing -> t
    fullSubs' set sigma (T.Arrow s m t u) =
      T.Arrow s m (fullSubs' set sigma t) (fullSubs' set sigma u)
    fullSubs' set sigma (T.Semi s t u) =
      T.Semi s (fullSubs' set sigma t) (fullSubs' set sigma u)
    fullSubs' set sigma (T.Message s p t) = T.Message s p (fullSubs' set sigma t)
    fullSubs' set sigma (T.Labelled p s tm) = T.Labelled p s $ Map.map (fullSubs' set sigma) tm
    fullSubs' set sigma (T.Forall s (Bind s1 x k t)) = T.Forall s $ Bind s1 x k (fullSubs' set sigma t) 
    fullSubs' set sigma (T.Dualof s t) = T.Dualof s (fullSubs' set sigma t) 
    fullSubs' _ _ t = t

quicklook :: Nary -> T.Type -> TypingState Sigma
quicklook n@(Nary h xs) res = do
  debugM $ "qlHead\n"
        ++ "\t* n:" ++ show n ++ "\n"
  qlHead h >>= \case
    Just sig -> do
      (args, res') <- inst sig xs
      mgu Map.empty (fromNary n) res res' 
    Nothing -> return Map.empty


qlHead :: E.Exp -> TypingState (Maybe T.Type)
qlHead (E.Var _ x) = getFromSignatures x >>= \case
  Just sig -> return $ Just sig
  Nothing  -> error $ show x ++ " is not in gamma"
  
-- qlHead (E.Int s _) = return $ Just $ T.Int s
-- qlHead (E.Char s _) = return $ Just $ T.Char s
qlHead _ = return Nothing -- error $ "QL Head does not apply " ++ show x


-- TODO: TO COMPLETE & This is only matching
mgu :: K.KindEnv -> E.Exp -> T.Type -> T.Type -> TypingState Sigma
mgu delta e (T.Forall _ (Bind _ x k1 t1)) (T.Forall _ (Bind _ y k2 t2)) = 
  mgu (Map.insert x k1 (Map.insert y k2 delta)) e t1 t2
mgu delta _ (T.Var _ x) t
  | Map.notMember x delta = pure $ Map.singleton x t
  | otherwise             = pure $ Map.empty
mgu delta _ t (T.Var _ x)
  | Map.notMember x delta = pure $ Map.singleton x t
  | otherwise             = pure $ Map.empty  
mgu delta e (T.Arrow _ m1 t1 t2) (T.Arrow _ m2 u1 u2) -- TODO: Check mults
 {- | m1 == m2-}              = Map.union <$> mgu delta e t1 u1 <*> mgu delta e t2 u2
--  | otherwise = undefined
mgu delta e (T.Message _ p1 t1) (T.Message _ p2 t2)
  | p1 == p2 = mgu delta e t1 t2
  | otherwise = undefined -- TODO: diff pols
mgu delta e (T.Semi _ t1 t2) (T.Semi _ u1 u2) =
  Map.union <$> mgu delta e t1 u1 <*> mgu delta e t2 u2

mgu delta e (T.Labelled _ s1 tm1) (T.Labelled _ s2 tm2)
  | s1 == s2 = -- Map.union <$> mgu delta e t1 u1 <*> mgu delta e t2 u2
      Map.foldlWithKey (\m k t -> mgu delta e t (tm2 Map.! k) >>=
                         \m2 -> m >>= \m1 -> pure $ Map.union m1 m2) (return Map.empty) tm1
mgu delta e (T.Dualof _ t) (T.Dualof _ u) = mgu delta e t u      
mgu delta e t u = pure Map.empty
  -- | equivalent t u = debugM (show t ++ " vs " ++ show u) $> Map.empty
  -- | otherwise = addError (TypeMismatch (getSpan t) t u e) $> Map.empty



synthHead kEnv (E.Var _ x) =
  getFromSignatures x >>= \case
    Just s -> do
      -- k <- K.synthetise kEnv s
      -- debugM $ "removing " ++ show x
--      when (K.isLin k) $ removeFromSignatures x
      return s

    Nothing -> do
      let p = getSpan x
          s = omission p
      addError (VarOrConsNotInScope p x)
      addToSignatures x s
      return s
-- synthHead kEnv (E.TypeApp _ e t) = do -- TODO: tmp      
--   u                             <- synthHead kEnv e
--   ~(T.Forall _ (Bind _ y k u')) <- Extract.forall e u
--   void $ K.checkAgainst kEnv k t
--   return $ Rename.subs t y u' 
synthHead kEnv e =
  debugM ("synthHead " ++ show e) >>
  synthetise kEnv e  -- error $ "synthHead " ++ show e
      

-- synthApp :: K.KindEnv ->  E.Exp -> TypingState T.Type
synthApp :: String -> K.KindEnv ->  E.Exp -> Maybe T.Type -> TypingState T.Type
synthApp mode kEnv e@E.App{} m = checkApp mode kEnv e m
synthApp mode kEnv e@E.Var{} m = checkApp mode kEnv e m
-- synthApp _ _ _ _ = return ()

-- checkApp :: K.KindEnv ->  E.Exp -> TypingState T.Type
checkApp :: String -> K.KindEnv ->  E.Exp -> Maybe T.Type -> TypingState T.Type
checkApp mode kEnv e m = do
  let (Nary h pi) = toNary e
  sigma <- synthHead kEnv h
  (phi, rho) <- inst (normalise sigma) pi

  theta <- case m of
             Just t -> mgu kEnv e t rho
             Nothing -> pure Map.empty

  let phi' = map (fullSubs theta . normalise) phi 
  let rho' = fullSubs theta rho 
  
  
  debugM $ "checkApp " ++ mode ++ " {\n"
        ++ "\t * nary:   " ++ show (Nary h pi) ++ "\n"
        ++ "\t * sigma:  " ++ show sigma ++ "\n"
        ++ "\t * phi: " ++ show phi ++ "\n"
        ++ "\t * rho: " ++ show rho ++ "\n"
        ++ "\t * theta: " ++ show theta ++ "\n"
        ++ "\t * phi': " ++ show phi' ++ "\n"
        ++ "\t * rho': " ++ show rho' ++ "\n"
        ++ "\t * kEnv: " ++ show kEnv ++ "\n"
        ++ "\t * theta: " ++ show theta ++ "\n"
        ++ "                        }\n"
  mapM_ (uncurry (checkArguments mode kEnv . fromNary)) (zip pi phi')
  
  when (isJust m) (
--    compareTypes e (remove rho') (remove $ fromJust m)
    compareTypes e (applyForall rho') (applyForall $ fullSubs theta $ fromJust m)
    )
  return rho'
  where
    applyForall t = let s = getSpan t in
      Map.foldlWithKey(\u x k -> if x `T.isFreeIn` u then T.Forall s (Bind s x k u) else u) t kEnv

    remove (T.Forall _ (Bind _ x k t)) = t
    remove t = t

checkArguments :: String -> K.KindEnv -> E.Exp -> T.Type -> TypingState ()
checkArguments mode kEnv e t = do
  debugM $ "checkArguments " ++ mode ++ " {\n"
        ++ "\t * e: " ++ show e ++ "\n"
        ++ "\t * t: " ++ show t ++ "\n"
        ++ "\t * kEnv: " ++ show kEnv ++ "\n"
        -- ++ "\t * kEnv: " ++ show kEnv ++ "\n"
        ++ "                              }\n"
  
--  let s = getSpan t
--  checkAgainst kEnv e t -- (Map.foldlWithKey (\u x k -> T.Forall s (Bind s x k u)) t kEnv)
  gen kEnv e t


gen kEnv e (T.Forall _ (Bind _ x k t)) = gen (Map.insert x k kEnv) e t
gen kEnv e t =
  case t of 
    t@(T.Arrow _ Lin t1 t2) -> do 
      (_, u1, u2) <- Extract.function e =<< synthetise kEnv e 
      compareTypes e u1 t1 
      compareTypes e u2 t2
    _ ->  checkAgainst kEnv e t
