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

{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Validation.Typing
  ( synthetise
  , checkAgainst
--  , fillFunType
  )
where


import           Equivalence.Equivalence
import           Parse.Unparser () -- debug
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Syntax.Value
import           Util.Error
import           Util.FreestState
import           Util.PreludeLoader ( userDefined ) -- debug
import           Util.Warning
import qualified Validation.Extract as Extract
import qualified Validation.Kinding as K -- Again?
import qualified Validation.Rename as Rename ( subs )

import           Control.Monad.State            ( when
                                                , unless, evalState, MonadState (get)
                                                )
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- SYNTHESISING A TYPE

synthetise :: K.KindEnv -> E.Exp -> FreestState T.Type
-- Basic expressions
synthetise _ (E.Int  p _  ) = return $ T.Int p
synthetise _ (E.Char p _  ) = return $ T.Char p
synthetise _ (E.Bool p _  ) = return $ T.Bool p
synthetise _ (E.Unit p    ) = return $ T.Unit p
synthetise _ (E.String p _) = return $ T.String p
  -- The 1st case is not strictly necessary but yields a better error message
synthetise kEnv e@(E.Var p x)
  | x == mkVar p "collect" = partialApplicationError e "channel of & type"
  | otherwise = synthetiseVar kEnv x
-- Unary let
synthetise kEnv (E.UnLet _ x e1 e2) = do
  t1 <- synthetise kEnv e1
  addToVEnv x t1
  t2 <- synthetise kEnv e2
  difference kEnv x
  return t2
-- Abs introduction
synthetise kEnv (E.Abs p Lin (Bind _ x t1 e)) = do
  void $ K.synthetise kEnv t1
  addToVEnv x t1
  t2 <- synthetise kEnv e
  difference kEnv x
  return $ T.Arrow p Lin t1 t2
synthetise kEnv e'@(E.Abs p Un (Bind _ x t1 e)) = do
  void $ K.synthetise kEnv t1
  vEnv1 <- getVEnv
  addToVEnv x t1
  t2 <- synthetise kEnv e
  difference kEnv x
  vEnv2 <- getVEnv
  checkEquivEnvs (getSpan e) "an unrestricted lambda" e' kEnv vEnv1 vEnv2
  return $ T.Arrow p Un t1 t2
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
  return $ T.Almanac p T.Variant $ Map.map (flip (T.Arrow p Un) (T.Unit defaultSpan)) tm
  -- Receive e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkVar p "receive" = do
  t        <- synthetise kEnv e
  (u1, u2) <- Extract.input e t
  void $ K.checkAgainst kEnv (K.lt $ defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  return $ T.Pair p u1 u2
  -- Send e1 e2
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkVar p "send" = do
  t        <- synthetise kEnv e2
  (u1, u2) <- Extract.output e2 t
  void $ K.checkAgainst kEnv (K.lt defaultSpan) u1
--  void $ K.checkAgainst kEnv (K.lm $ pos u1) u1
  checkAgainst kEnv e1 u1
  return u2
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
-- Type application
synthetise kEnv (E.TypeApp _ e t) = do
  u                               <- synthetise kEnv e
  ~(T.Forall _ (Bind _ y k u')) <- Extract.forall e u
  void $ K.checkAgainst kEnv k t
  return $ Rename.subs t y u'
-- Boolean elimination
synthetise kEnv e'@(E.Cond p e1 e2 e3) = do
  checkAgainst kEnv e1 (T.Bool p)
  vEnv2 <- getVEnv
  t     <- synthetise kEnv e2
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p "a conditional" e' kEnv vEnv3 vEnv4
  return t
-- Pair introduction
synthetise kEnv (E.Pair p e1 e2) = do
  t1 <- synthetise kEnv e1
  t2 <- synthetise kEnv e2
  return $ T.Pair p t1 t2
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
synthetise kEnv (E.Case p e fm) = synthetiseCase p kEnv e fm
-- Session types
synthetise kEnv (E.New p t u) = do
  K.checkAgainstSession kEnv t
  return $ T.Pair p t u

-- | Returns the type of a variable; removes it from vEnv if lin

synthetiseVar :: K.KindEnv -> Variable -> FreestState T.Type
synthetiseVar kEnv x = getFromVEnv x >>= \case
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

partialApplicationError :: E.Exp -> String -> FreestState T.Type
partialApplicationError e s =
  let p = getSpan e in addError (PartialApplied p e s) $> omission p

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> FreestState ()
-- Boolean elimination
checkAgainst kEnv e@(E.Cond p e1 e2 e3) t = do
  checkAgainst kEnv e1 (T.Bool p)
  vEnv2 <- getVEnv
  checkAgainst kEnv e2 t
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p "a conditional" e kEnv vEnv3 vEnv4
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
checkAgainst kEnv e t = checkEquivTypes e kEnv t =<< synthetise kEnv e

-- EQUALITY EQUIVALENCE CHECKING

checkEquivTypes :: E.Exp -> K.KindEnv -> T.Type -> T.Type -> FreestState ()
checkEquivTypes exp kEnv expected actual =
  unless (equivalent kEnv actual expected) $
    addError (NonEquivTypes (getSpan exp) expected actual exp)

checkEquivEnvs
  :: Span -> String -> E.Exp -> K.KindEnv -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p branching exp kEnv vEnv1 vEnv2 = do
  let vEnv1' = userDefined vEnv1
      vEnv2' = userDefined vEnv2
  unless (equivalent kEnv vEnv1' vEnv2') $
    addError (NonEquivEnvs p branching (vEnv1' Map.\\ vEnv2') (vEnv2' Map.\\ vEnv1') exp)

synthetiseCase :: Span -> K.KindEnv -> E.Exp -> E.FieldMap -> FreestState T.Type
synthetiseCase p kEnv e fm  = do
  fm'  <- buildMap p fm =<< Extract.datatypeMap e =<< synthetise kEnv e
  vEnv <- getVEnv
  ~(t : ts, v : vs) <- Map.foldr (synthetiseMap kEnv vEnv)
                                 (return ([], [])) fm'
  mapM_ (checkEquivTypes e kEnv t)           ts
  mapM_ (checkEquivEnvs p "a case" e kEnv v) vs
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


-- Building abstractions for each case element

buildMap :: Span -> E.FieldMap -> T.TypeMap -> FreestState E.FieldMap
buildMap p fm tm = do
  when (tmS /= fmS && tmS > fmS) $ addWarning (NonExhaustiveCase p fm tm)
  tMapWithKeyM (buildAbstraction tm) fm
  where tmS = Map.size tm
        fmS = Map.size fm

buildAbstraction :: T.TypeMap -> Variable -> ([Variable], E.Exp)
                 -> FreestState ([Variable], E.Exp)
buildAbstraction tm x (xs, e) = case tm Map.!? x of
  Just t -> let n = numberOfArgs t in
    if n /= length xs
      then addError (WrongNumOfCons (getSpan e) x n xs e) $> (xs, e)
      else return (xs, buildAbstraction' (xs, e) t)
  Nothing -> -- Data constructor not in scope
    addError (DataConsNotInScope (getSpan x) x) $> (xs, e)
 where
  buildAbstraction' :: ([Variable], E.Exp) -> T.Type -> E.Exp
  buildAbstraction' ([], e) _ = e
  buildAbstraction' (x : xs, e) (T.Arrow _ _ t1 t2) =
    E.Abs (getSpan e) Lin $ Bind (getSpan e) x t1 $ buildAbstraction' (xs, e) t2
  buildAbstraction' ([x], e) t = E.Abs (getSpan e) Lin $ Bind (getSpan e) x t e

  numberOfArgs :: T.Type -> Int
  numberOfArgs (T.Arrow _ _ _ t) = 1 + numberOfArgs t
  numberOfArgs _                 = 0
