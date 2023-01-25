{-|
Module      :  Validation.Typing
Description :  Checking the good formation of expressions
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

A bidirectional type system.
-}

{-# LANGUAGE LambdaCase #-}

module Validation.Typing
  ( synthetise
  , checkAgainst
  )
where

import           Parse.Unparser () -- debug
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Syntax.Value
import           Util.Error
import           Util.FreestState
import           Util.Warning
import           Bisimulation.Bisimulation ( bisimilar )
import qualified Validation.Extract as Extract
import qualified Validation.Kinding as K -- K Again?
import qualified Validation.Rename as Rename ( subs )

import           Control.Monad.State ( when
                                     , unless, evalState, MonadState (get)
                                     )
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Parse.ParseUtils (tupleTypeMap)

synthetise :: K.KindEnv -> E.Exp -> FreestState T.Type
-- Basic expressions
synthetise _ (E.Int  p _  ) = return $ T.Int p
synthetise _ (E.Char p _  ) = return $ T.Char p
synthetise _ (E.Unit p    ) = return $ T.unit p
synthetise _ (E.String p _) = return $ T.String p
  -- The 1st case is not strictly necessary but yields a better error message
synthetise kEnv e@(E.Var p x)
  | x == mkVar p "collect" =
    addError (PartialApplied p e "channel of & type") $> omission p
  | otherwise =  getFromVEnv x >>= \case
    Just s -> do
      k <- K.synthetise kEnv s
      when (K.isLin k) $ removeFromVEnv x
      return s
    Nothing -> do
      let p = getSpan x
          s = omission p
      addError (VarOrConsNotInScope p x)
      addToVEnv x s
      return s
-- Unary let
synthetise kEnv (E.UnLet _ x e1 e2) = do
  t1 <- synthetise kEnv e1
  addToVEnv x t1
  t2 <- synthetise kEnv e2
  difference kEnv x
  return t2
-- Abstraction
synthetise kEnv e'@(E.Abs p mult (Bind _ x t1 e)) = do
  void $ K.synthetise kEnv t1
  vEnv1 <- getVEnv -- Redundant when mult == Lin
  addToVEnv x t1
  t2 <- synthetise kEnv e
  difference kEnv x
  when (mult == Un) (do
    vEnv2 <- getVEnv
    checkEquivEnvs (getSpan e) NonEquivEnvsInUnFun e' kEnv vEnv1 vEnv2)
  return $ T.Arrow p mult t1 t2
-- Application, the special cases first
  -- Select C e
synthetise kEnv (E.App p (E.App _ (E.Var _ x) (E.Var _ c)) e)
  | x == mkVar p "select" = do
    t <- synthetise kEnv e
    m <- Extract.inChoiceMap e t
    Extract.choiceBranch p m c t
  -- Collect e
synthetise kEnv (E.App _ (E.Var p x) e) | x == mkVar p "collect" = do
  tm <- Extract.outChoiceMap e =<< synthetise kEnv e
  return $ T.Almanac p T.Variant
    (Map.map (T.Almanac p T.Record . Map.singleton (mkVar p "0")) tm)
  -- Receive e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkVar p "receive" = do
  t        <- synthetise kEnv e
  (u1, u2) <- Extract.input e t
  void $ K.checkAgainst kEnv (K.lt $ defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  return $ T.Almanac p T.Record $ tupleTypeMap [u1, u2]
  -- Send e1 e2
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkVar p "send" = do
  t        <- synthetise kEnv e2
  (u1, u2) <- Extract.output e2 t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  checkAgainst kEnv e1 u1
  return u2
  -- Close e1
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkVar p "close" = do
  t <- Extract.end e =<< synthetise kEnv e
  return $ T.unit p
  -- Fork e
synthetise kEnv (E.App p fork@(E.Var _ x) e) | x == mkVar p "fork" = do
  (_, t) <- get >>= \s -> Extract.function e (evalState (synthetise kEnv e) s)
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
synthetise kEnv (E.TypeApp p new@(E.Var _ x) t) | x == mkVar p "new" = do
  unless (broughtToEnd t) (addError (UnendedSession p t))
  u                             <- synthetise kEnv new
  ~(T.Forall _ (Bind _ y k u')) <- Extract.forall new u
  -- TODO: is there a better way of doing this for `new`?
  -- check against a new 'Endable' kind?
  void $ K.checkAgainst kEnv k t
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
  return $ T.Almanac p T.Record $ Map.fromList [(mkVar (getSpan t1) "0", t1)
                                               ,(mkVar (getSpan t2) "1", t2)]
-- Pair elimination
synthetise kEnv (E.BinLet _ x y e1 e2) = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToVEnv x u1
  addToVEnv y u2
  t2 <- synthetise kEnv e2
  difference kEnv x
  difference kEnv y
  return t2
-- Datatype elimination
synthetise kEnv (E.Case p e fm) = do
  fm'  <- buildMap p fm =<< Extract.datatypeMap e =<< synthetise kEnv e
  vEnv <- getVEnv
  ~(t : ts, v : vs) <- Map.foldr (synthetiseMap kEnv vEnv)
                                 (return ([], [])) fm'
  mapM_ (checkEquivTypes e kEnv t) ts
  mapM_ (checkEquivEnvs p NonEquivEnvsInBranch e kEnv v) vs
  setVEnv v
  return t

synthetiseMap :: K.KindEnv -> VarEnv -> ([Variable], E.Exp)
              -> FreestState ([T.Type], [VarEnv])
              -> FreestState ([T.Type], [VarEnv])
synthetiseMap kEnv vEnv (xs, e) state = do
  (ts, envs) <- state
  t          <- synthetise kEnv e
  env        <- getVEnv
  setVEnv vEnv
  return (returnType xs t : ts, env : envs)
 where
  returnType :: [Variable] -> T.Type -> T.Type
  returnType [] t                  = t
  returnType (_:xs) (T.Arrow _ _ _ t2) = returnType xs t2
  returnType _ t                  = t

-- The difference operation. Removes a program variable from the
-- variable environment and gives an error if it is linear
difference :: K.KindEnv -> Variable -> FreestState ()
difference kEnv x = do
  getFromVEnv x >>= \case
    Just t -> do
      k <- K.synthetise kEnv t
      when (K.isLin k) $ addError (LinProgVar (getSpan x) x t k)
    Nothing -> return ()
  removeFromVEnv x

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> FreestState ()
-- Pair elimination
checkAgainst kEnv (E.BinLet _ x y e1 e2) t2 = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToVEnv x u1
  addToVEnv y u2
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
checkAgainst kEnv e (T.Arrow _ Lin t u) = do
  (t', u') <- Extract.function e =<< synthetise kEnv e
  checkEquivTypes e kEnv t' t
  checkEquivTypes e kEnv u' u
checkAgainst kEnv e t = checkEquivTypes e kEnv t =<< synthetise kEnv e

checkEquivTypes :: E.Exp -> K.KindEnv -> T.Type -> T.Type -> FreestState ()
checkEquivTypes exp kEnv expected actual =
  -- unless (equivalent kEnv actual expected) $
  unless (bisimilar actual expected) $
    addError (NonEquivTypes (getSpan exp) expected actual exp)

checkEquivEnvs :: Span -> (Span -> VarEnv -> VarEnv -> E.Exp -> ErrorType) -> E.Exp -> K.KindEnv -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p error exp kEnv vEnv1 vEnv2 =
  -- unless (equivalent kEnv vEnv1 vEnv2) $
  unless (Map.keysSet vEnv1 == Map.keysSet vEnv2) $
    addError (error p (vEnv1 Map.\\ vEnv2) (vEnv2 Map.\\ vEnv1) exp)

-- Build abstractions for each case element

buildMap :: Span -> E.FieldMap -> T.TypeMap -> FreestState E.FieldMap
buildMap p fm tm = do
  when (tmS /= fmS && tmS > fmS) $ addWarning (NonExhaustiveCase p fm tm)
  tMapWithKeyM (buildAbstraction tm) fm
  where tmS = Map.size tm
        fmS = Map.size fm

buildAbstraction :: T.TypeMap -> Variable -> ([Variable], E.Exp)
                 -> FreestState ([Variable], E.Exp)
buildAbstraction tm x (xs, e) = case tm Map.!? x of
  Just (T.Almanac _ T.Record rtm) -> let n = Map.size rtm in
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


  numberOfArgs :: T.Type -> Int
  numberOfArgs (T.Arrow _ _ _ t) = 1 + numberOfArgs t
  numberOfArgs _                 = 0

  numberOfFields :: T.Type -> Int
  numberOfFields (T.Almanac _ _  tm) = Map.size tm

-- Check whether a type is brought to an End
broughtToEnd :: T.Type -> Bool
broughtToEnd = wellEnded Set.empty

wellEnded :: Set.Set Variable -> T.Type -> Bool
wellEnded _ T.Skip{} = False
wellEnded _ T.End{} = True
wellEnded s (T.Semi _ t1 t2) = wellEnded s t1 || wellEnded s t2
wellEnded _ T.Message{} = False
wellEnded s (T.Almanac _ _ m) = Map.foldr (\t b -> b && wellEnded s t) True m
wellEnded s (T.Rec _ (Bind{var=v, body=t})) = wellEnded (Set.insert v s) t
wellEnded s (T.Dualof _ t) = wellEnded s t

-- Alternative 1 _ Only recursion variables are well ended (False negatives)
-- There are non well-formed functions in the Prelude (e.g., forkWith)
-- 327 examples, 213 failures, 12 pending

-- wellEnded s (T.Var _ var) = var `Set.member` s
-- wellEnded s (T.CoVar _ var) = var `Set.member` s -- ???

-- Alternative 2 _ All type variables are well ended (False positives)
-- Allows false positives: forkWith @Skip @Skip (id @Skip)
-- 327 examples, 43 failures, 12 pending

wellEnded _ (T.Var _ _) = True
-- wellEnded s (T.CoVar _ _) = True
