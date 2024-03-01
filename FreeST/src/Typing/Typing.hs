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

import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.State (evalState, evalStateT, MonadState (get), liftIO)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           System.Timeout (timeout)
import Parse.Read

typeCheck :: TypingState ()
typeCheck = do
  -- * Check the formation of all type decls
  mapM_ (uncurry $ K.checkAgainst Map.empty) =<< getTypes
  -- * Check the formation of all function signatures
  mapM_ (K.synthetise Map.empty) =<< getSignatures
  -- Gets the state and only continues if there are no errors so far
  s <- get
  unless (hasErrors s) $ do
    -- * Check function bodies
    tMapWithKeyM_ (checkFunBody (signatures $ ast s)) =<< getDefs
    -- * Check the main function
    checkMainFunction
    -- * Checking final environment for linearity
    checkLinearity

-- Check a given function body against its type; make sure all linear
-- variables are used.
checkFunBody :: Signatures -> Variable -> E.Exp -> TypingState ()
checkFunBody sigs f e =
  forM_ (sigs Map.!? f) (checkAgainst Map.empty e)

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

buildAbstraction :: T.TypeMap -> Variable -> ([Variable], E.Exp)
                 -> TypingState ([Variable], E.Exp)
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


  -- numberOfArgs :: T.Type -> Int
  -- numberOfArgs (T.Arrow _ _ _ t) = 1 + numberOfArgs t
  -- numberOfArgs _                 = 0

  -- numberOfFields :: T.Type -> Int
  -- numberOfFields (T.Labelled _ _  tm) = Map.size tm

--


synthetise :: K.KindEnv -> E.Exp -> TypingState T.Type
-- Basic expressions
synthetise _ (E.Int  p _  ) = return $ T.Int p
synthetise _ (E.Float p _ ) = return $ T.Float p
synthetise _ (E.Char p _  ) = return $ T.Char p
synthetise _ (E.Unit p    ) = return $ T.unit p
synthetise _ (E.String p _) = return $ T.String p
synthetise kEnv (E.Var _ x) =
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
synthetise kEnv (E.App p fork@(E.Var _ x) e) | x == mkFork p = do
  s <- get 
  u <- liftIO $ evalStateT (synthetise kEnv e) s
  (_, t) <- Extract.function e u
  synthetise kEnv (E.App p (E.TypeApp p fork t) e)
-- Application, general case
synthetise kEnv (E.App _ e1 e2) = do
  t        <- synthetise kEnv e1
  (u1, u2) <- Extract.function e1 t
  checkAgainst kEnv e2 u1
  return u2
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
-- checkAgainst kEnv (App p e1 e2) u = do
--   t <- synthetise kEnv e2
--   checkAgainst kEnv e1 (Fun p Un/Lin t u)
checkAgainst kEnv e t = do 
  u   <- synthetise kEnv e
  compareTypes e t u 

compareTypes :: E.Exp -> T.Type -> T.Type -> TypingState () 
compareTypes e t u = do 
  sub <- subtyping <$> getRunOpts
  timeout_ms   <- checkTimeout_ms <$> getRunOpts
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

buildMap :: Span -> E.FieldMap -> T.TypeMap -> TypingState E.FieldMap
buildMap p fm tm = do
  when (tmS /= fmS && tmS > fmS) $ addWarning (NonExhaustiveCase p fm tm)
  tMapWithKeyM (buildAbstraction tm) fm
  where tmS = Map.size tm
        fmS = Map.size fm
