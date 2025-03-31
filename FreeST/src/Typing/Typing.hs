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
  ( typeCheck
  , synthetise -- for tests
  , checkAgainst -- for tests
  , checkDefs
  , buildMap
  )
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
import           Restriction.Restriction

import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.State ( evalState, MonadState (get), evalStateT, liftIO)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           System.Timeout (timeout)
import qualified Data.Set as Set


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
    -- * Check the set of level inequalities
    checkInequalities
    
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
          (k, _) <-  K.synthetise Map.empty t 
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
      (k, _) <-  K.synthetise Map.empty t 
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
      (k, _) <- K.synthetise Map.empty t
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


synthetise :: K.KindEnv -> E.Exp -> TypingState (T.Type, T.Level)
-- Basic expressions
synthetise _ (E.Int  p _  ) = return $ (T.Int p, level $ T.Int p)
synthetise _ (E.Float p _ ) = return $ (T.Float p, level $ T.Float p)
synthetise _ (E.Char p _  ) = return $ (T.Char p, level $ T.Char p)
synthetise _ (E.Unit p    ) = return $ (T.unit p, T.Bottom)
synthetise _ (E.String p _) = return $ (T.String p, level $ T.String p)
synthetise kEnv (E.Var _ x) =
  getFromSignatures x >>= \case
    Just s -> do
      (k, l) <- K.synthetise kEnv s
      when (K.isLin k) $ removeFromSignatures x
      return (s, l)
    Nothing -> do
      let p = getSpan x
          s = omission p
      addError (VarOrConsNotInScope p x)
      addToSignatures x s
      return (s, T.Bottom)
-- Unary let
synthetise kEnv (E.UnLet _ x e1 e2) = do
  (t1, l1) <- synthetise kEnv e1
  addToSignatures x t1
  (t2, l2) <- synthetise kEnv e2
  difference kEnv x
  -- -- addInequality (getSpan t1) (l1, l2)
  -- return (t2, l2)
  -- addInequality (getSpan t2) (l1, level t2)
  return (t2, joinLevels l1 l2)
-- Abstraction
synthetise kEnv e'@(E.Abs p mult (Bind _ x t1 e)) = do
  void $ K.synthetise kEnv t1
  sigs1 <- getSignatures -- Redundant when mult == Lin
  addToSignatures x t1
  (t2, _) <- synthetise kEnv e
  difference kEnv x
  when (mult == Un) (do
    sigs2 <- getSignatures
    checkEquivEnvs (getSpan e) NonEquivEnvsInUnFun e' kEnv sigs1 sigs2) 
  let l1 = level t1 in
    let l2 = level t2 in
      -- return (T.Arrow p mult l1 l2 t1 t2, level $ T.Arrow p mult l1 l2 t1 t2)
      return (T.Arrow p mult l1 l2 t1 t2, T.Bottom)
-- Application, the special cases first
  -- Select C e
synthetise kEnv (E.App p (E.App _ (E.Var _ x) (E.Var _ c)) e)
  | x == mkSelect p = do
    (t, l) <- synthetise kEnv e
    m <- Extract.inChoiceMap e t
    t1 <- Extract.choiceBranch p m c t
    addInequality (getSpan t) (l, level t1)
    -- return (t1, level t1)
    return (t1, T.Bottom)
  -- Collect e
synthetise kEnv (E.App _ (E.Var p x) e) | x == mkCollect p = do
  (t, _) <- synthetise kEnv e
  tm <- Extract.outChoiceMap e t
  -- let l = level $ T.Labelled p T.Variant T.Bottom $
  --          Map.map (T.Labelled p T.Record T.Bottom . Map.singleton (head mkTupleLabels p)) tm
  -- return (T.Labelled p T.Variant T.Bottom
  --         (Map.map (T.Labelled p T.Record T.Bottom . Map.singleton (head mkTupleLabels p)) tm), l)
  return (T.Labelled p T.Variant T.Bottom
          (Map.map (T.Labelled p T.Record T.Bottom . Map.singleton (head mkTupleLabels p)) tm), T.Bottom)
  -- Receive e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkReceive p = do
  (t, l)        <- synthetise kEnv e
  (u1, u2) <- Extract.input e t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
  addInequality (getSpan t) (l, level u1)
  -- addInequality (getSpan t) (l, level u2)
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  return (T.tuple p [u1, u2], l)
  -- Send e1 e2
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkSend p = do
  (t, l)     <- synthetise kEnv e2
  (u1, u2) <- Extract.output e2 t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
  addInequality (getSpan t) (l, level u1)
  -- addInequality (getSpan t) (l, level u2)
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  checkAgainst kEnv e1 u1
  return (u2, l)
  -- fork e
synthetise kEnv (E.App p fork@(E.Var _ x) e) | x == mkFork p = do
  s <- get
  (u, _) <- liftIO $ evalStateT (synthetise kEnv e) s
  (_, _, t) <- Extract.function e u
  synthetise kEnv (E.App p (E.TypeApp p fork t) e)
-- Application, general case
synthetise kEnv (E.App _ e1 e2) = do
  (t, l1)      <- synthetise kEnv e1
  (_, u1, u2) <- Extract.function e1 t
  checkAgainst kEnv e2 u1
  -- addInequality (getSpan t) (l1, level u2)
  return (u2, level u2)
-- Type abstraction
synthetise kEnv e@(E.TypeAbs _ (Bind p a k e')) = do
  unless (isVal e') (addError (TypeAbsBodyNotValue (getSpan e') e e'))
  (t, _) <- synthetise (Map.insert a k kEnv) e'
  return (T.Forall p (Bind p a k t), T.Bottom) --forall has no priorities for now
-- New @t - check that t comes to an End
synthetise kEnv (E.TypeApp p new@(E.Var _ x) t) | x == mkNew p = do
  (u, _)                           <- synthetise kEnv new
  ~(T.Forall _ (Bind _ y _ u')) <- Extract.forall new u
  void $ K.checkAgainstAbsorb kEnv t
  -- return (Rename.subs t y u', level $ Rename.subs t y u')
  return (Rename.subs t y u', T.Bottom)
-- Type application
synthetise kEnv (E.TypeApp _ e t) = do
  (u, _)                            <- synthetise kEnv e
  ~(T.Forall _ (Bind _ y k u')) <- Extract.forall e u
  void $ K.checkAgainst kEnv k t
  -- return (Rename.subs t y u', level $ Rename.subs t y u')
  return (Rename.subs t y u', T.Bottom)
-- Pair introduction
synthetise kEnv (E.Pair p e1 e2) = do
  (t1, l1) <- synthetise kEnv e1
  (t2, l2) <- synthetise kEnv e2
  -- -- addInequality (getSpan t1) (l1, l2) --change this
  let l1 = level t1
  let l2 = level $ T.Labelled p T.Record l1 $ Map.fromList (zipWith (\ml t -> (ml $ getSpan t, t)) mkTupleLabels [t1, t2])
  return (T.Labelled p T.Record l1 $
    Map.fromList (zipWith (\ml t -> (ml $ getSpan t, t)) mkTupleLabels [t1, t2]), l2)
-- Pair elimination
synthetise kEnv (E.BinLet _ x y e1 e2) = do
  (t1, l1)    <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToSignatures x u1
  addToSignatures y u2
  (t2, l2) <- synthetise kEnv e2
  difference kEnv x
  difference kEnv y
  -- -- addInequality (getSpan t1) (l1, l2)
  -- return (t2, l2)
  -- addInequality (getSpan t1) (l1, level t2)
  return (t2, joinLevels l1 l2)
-- Datatype elimination
synthetise kEnv (E.Case p e fm) = do
  (t1, _) <- synthetise kEnv e
  fm'  <- buildMap p fm =<< Extract.datatypeMap e t1
  sigs <- getSignatures
  ~(t : ts, v : vs) <- Map.foldr (synthetiseMap kEnv sigs)
                                 (return ([], [])) fm'
  mapM_ (compareTypes e t) ts
  mapM_ (checkEquivEnvs p NonEquivEnvsInBranch e kEnv v) vs
  setSignatures v
  -- return (t, level t)
  return (t, T.Bottom)

synthetiseMap :: K.KindEnv -> Signatures -> ([Variable], E.Exp)
              -> TypingState ([T.Type], [Signatures])
              -> TypingState ([T.Type], [Signatures])
synthetiseMap kEnv sigs (xs, e) state = do
  (ts, envs) <- state
  (t, _)     <- synthetise kEnv e
  env        <- getSignatures
  setSignatures sigs
  return (returnType xs t : ts, env : envs)
 where
  returnType :: [Variable] -> T.Type -> T.Type
  returnType [] t                  = t
  returnType (_:xs) (T.Arrow _ _ _ _ _ t2) = returnType xs t2
  returnType _ t                  = t

-- The difference operation. Removes a program variable from the
-- variable environment and gives an error if it is linear
difference :: K.KindEnv -> Variable -> TypingState ()
difference kEnv x = do
  getFromSignatures x >>= \case
    Just t -> do
      (k, _) <- K.synthetise kEnv t
      when (K.isLin k) $ addError (LinProgVar (getSpan x) x t k)
    Nothing -> return ()
  removeFromSignatures x

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> TypingState ()
-- checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> T.Level -> TypingState ()

-- Pair elimination
checkAgainst kEnv (E.BinLet _ x y e1 e2) t2 = do
-- checkAgainst kEnv (E.BinLet _ x y e1 e2) t2 l = do
  (t1, l1)  <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToSignatures x u1
  addToSignatures y u2
  checkAgainst kEnv e2 t2
  difference kEnv x
  difference kEnv y
  addInequality (getSpan t1) (l1, level t2)
-- TODO Datatype elimination
-- checkAgainst kEnv (Case p e fm) = checkAgainstFieldMap p kEnv e fm Extract.datatypeMap
-- Abs elimination. It seems that we cannot do checkAgainst for we
-- cannot decide whether to use a Lin or a Un function. See
-- counterexamples: polySUTL.fst when using Lin, and mult.fst when
-- using Un
-- checkAgainst kEnv (App p e1 e2) u = do
--   t <- synthetise kEnv e2
--   checkAgainst kEnv e1 (Fun p Un/Lin t u)
checkAgainst kEnv e t = do 
-- checkAgainst kEnv e t l = do 
  sub <- subtyping <$> getRunOpts
  case t of 
    t@(T.Arrow _ Lin _ _ t1 t2) | not sub -> do 
      (t3, l3) <- synthetise kEnv e
      (_, u1, u2) <- Extract.function e t3 
      compareTypes e u1 t1 
      compareTypes e u2 t2  
      addInequality (getSpan t) (l3, level t)
    _ -> do 
      (t4, l4) <- synthetise kEnv e
      compareTypes e t t4
      -- addInequality (getSpan t) (l4, level t)
    --compare levels

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
  Just (T.Labelled _ T.Record _ rtm) -> let n = Map.size rtm in
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

-- checkInequalities :: TypingState ()
-- checkInequalities = do
--   ineq <- getInequalities
--   forM_ ineq $ \(l1, l2) -> do
--     unless (isValidIneq l1 l2) $
--       addError (LevelMismatch defaultSpan l1 l2) --need a span for this error but don't have one

checkInequalities :: TypingState ()
checkInequalities = do
  ineq <- getInequalities
  forM_ (Set.toList ineq) $ \(span, (l1, l2)) -> do
    unless (isValidIneq l1 l2) $
      addError (LevelMismatch span l1 l2) -- Use the span from the inequality

isValidIneq :: T.Level -> T.Level -> Bool
isValidIneq T.Top T.Top = True
isValidIneq T.Top _ = True
isValidIneq _ T.Top = False
isValidIneq T.Bottom T.Bottom = True
isValidIneq _ T.Bottom = True
isValidIneq T.Bottom _ = False
isValidIneq (T.Num n1) (T.Num n2) = n1 < n2 -- <= or < ?
--isValidIneq _ _ = False