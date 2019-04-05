{-|
Module      :  TypingExps
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

-- TODO: remove NoMonadFailDesugaring and add an instance monad fail
module Validation.TypingExps
( synthetise
, checkAgainst
, checkAgainstST
, checkUn
, fillFunType
, funSigsOnly
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import           Utils.PreludeLoader (isBuiltin)
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Control.Monad.State
import           Control.Conditional ((<&&>))
import qualified Data.Map.Strict as Map
import           Validation.Extract
import           Debug.Trace

-- SYNTHESIS

synthetise :: Expression -> FreestState Type
-- Basic expressions
synthetise (Unit p)        = return $ Basic p UnitType
synthetise (Integer p _)   = return $ Basic p IntType
synthetise (Character p _) = return $ Basic p CharType
synthetise (Boolean p _)   = return $ Basic p BoolType
-- Variable
synthetise (ProgVar p x) = do
  s@(TypeScheme _ bs t) <- synthetiseVar p x
  when (not $ null bs) 
    (addError p ["Variable", styleRed x, "of a polymorphic type used in a monomorphic context\n",
                 "\t type scheme:", styleRed $ show s])
  return t
synthetise (UnLet _ x e1 e2) = do
  t1 <- synthetise e1
  addToVenv x (TypeScheme (position t1) [] t1)
  t2 <- synthetise e2
  quotient x
  return t2
-- Abstraction introduction
synthetise (Lambda p m x t1 e) = do
  venv1 <- getVenv
  K.synthetise t1
  addToVenv x (toTypeScheme t1)
  t2 <- synthetise e
  quotient x
  venv2 <- getVenv
  when (m == Un) (checkEqualEnvs p venv1 venv2)
  return $ Fun p m t1 t2
-- Abstraction elimination
synthetise (App _ e1 e2) = do
  t <- synthetise e1
  (u1, u2) <- extractFun t
  checkAgainst e2 u1
  return u2
-- Type application
synthetise (TypeApp p x ts) = do
  (TypeScheme _ bs t) <- synthetiseVar p x
  when (length ts /= length bs) 
    (addError p ["Wrong number of arguments to type application\n",
                "\t parameters:", styleRed $ show bs, "\n",
                "\t arguments: ", styleRed $ show ts])  
  let typeBinds = zip ts bs
  mapM (\(t, TBindK _ _ k) -> K.checkAgainst k t) typeBinds
  return $ foldr (uncurry subs) t typeBinds
-- Boolean elimination
synthetise (Conditional p e1 e2 e3) = do
  checkAgainst e1 (Basic p BoolType)
  venv2 <- getVenv
  t <- synthetise e2
  venv3 <- getVenv
  setVenv venv2
  checkAgainst e3 t
  venv4 <- getVenv
  checkEquivEnvs p venv3 venv4
  return t
-- Pair introduction
synthetise (Pair p e1 e2) = do
  t1 <- synthetise e1
  t2 <- synthetise e2
  return $ PairType p t1 t2
-- Pair elimination
synthetise (BinLet _ x y e1 e2) = do
  t <- synthetise e1
  (u1,u2) <- extractPair t
  addToVenv x (TypeScheme (position u1) [] u1)
  addToVenv y (TypeScheme (position u2) [] u2)
  u <- synthetise e2
  venv <- getVenv
  quotient x
  quotient y
  return u
-- Fork
synthetise (Fork p e) = do
  t <- synthetise e
  checkUn (toTypeScheme t)
  return $ Basic p UnitType
-- Session types
synthetise (New p t) = do
  K.checkAgainst (Kind p Session Lin) t
  return $ PairType p t (Dualof p t)
synthetise (Send p e) = do
  t <- synthetise e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)
synthetise (Receive p e) = do
  t <- synthetise e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2
synthetise (Select p c e) = do
  t <- synthetise e
  m <- extractOutChoiceMap p t
  extractCons p m c
synthetise (Match p e fm) = synthetiseFieldMap p e fm extractInChoiceMap
-- Datatype elimination
synthetise (Case p e fm) = synthetiseFieldMap p e fm extractDatatypeMap

-- | Returns the type scheme for a variable; removes it from venv if lin
synthetiseVar :: Pos -> PVar -> FreestState TypeScheme
synthetiseVar p x = do
  let b = PBind p x
  getFromVenv b >>= \case
    Just ts -> do
      k <- K.synthetiseTS ts
      when (isLin k) $ removeFromVenv b
      return ts
    Nothing -> do
      addError p ["Variable or data constructor not in scope:", styleRed x]
      let ts = omission p
      addToVenv b ts
      return ts

synthetiseFieldMap :: Pos -> Expression -> FieldMap ->
  (Pos -> Type -> FreestState TypeMap) -> FreestState Type
synthetiseFieldMap p e fm extract = do
  t <- synthetise e
  tm <- extract p t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (synthetiseField venv tm) (return ([],[])) fm
  mapM_ (checkEquivTypes p t) ts
  mapM_ (checkEquivEnvs p v) vs
  setVenv v
  return t

-- Checks either the case map and the match map (all the expressions)
synthetiseField :: VarEnv -> TypeMap -> PBind -> Expression ->
  FreestState ([Type], [VarEnv]) -> FreestState ([Type], [VarEnv])
synthetiseField venv1 tm b e state = do
  (ts, venvs) <- state
  setVenv venv1
  t1 <- synthetiseCons b tm
  trace ("A synthetiseField: " ++ show e) (return ())
  t2 <- fillFunType b e (toTypeScheme t1)
--  trace ("B synthetiseField: " ++ show e' ++ ": " ++ show (returnType t)) (return ())
--  t2 <- synthetise e'
  venv2 <- getVenv
  return (t2:ts, venv2:venvs)

-- Check whether a constructor exists in a type map
synthetiseCons :: PBind -> TypeMap -> FreestState Type
synthetiseCons b@(PBind p c) tm = do
  case tm Map.!? b of
    Just t  -> return t
    Nothing -> do
      addError p ["Data constructor or choice field", styleRed c, "not in scope"]
      return $ Skip p

-- | Adds a list of binds to KindEnv
-- addBindsToKenv :: Pos -> [TBindK] -> FreestState ()
-- addBindsToKenv p = mapM_ (\(TBindK _ b k) -> addToKenv (TBind p b) k)

-- | The quotient operation
-- removes a variable from the environment and gives an error if it is linear
quotient :: PBind -> FreestState ()
quotient b =
  getFromVenv b >>= \case
    Just t  -> checkUn t >> removeFromVenv b
    Nothing -> return ()

-- | Check whether a type is unrestricted
checkUn :: TypeScheme -> FreestState ()
checkUn ts = do
  k <- K.synthetiseTS ts
  when (isLin k) $
    addError (position ts) ["Linear variable at the end of its scope",
                            styleRed (show ts), ":", styleRed (show k)]

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: Expression -> Type -> FreestState ()
-- Boolean elimination
checkAgainst (Conditional p e1 e2 e3) t = do
  checkAgainst e1 (Basic p BoolType)
  venv2 <- getVenv
  checkAgainst e2 t
  venv3 <- getVenv
  setVenv venv2
  checkAgainst e3 t
  venv4 <- getVenv
  checkEquivEnvs p venv3 venv4
-- Pair elimination
  -- TODO
-- Default
checkAgainst e t = do
  u <- synthetise e
  checkEquivTypes (position e) t u

-- | Check an expression against a given type scheme
checkAgainstST :: Expression -> TypeScheme -> FreestState ()
checkAgainstST e (TypeScheme _ bs t) = do
  mapM_ (\(TBindK p x k) -> addToKenv (TBind p x) k) bs
  checkAgainst e t
  
-- EQUIVALENCE

checkEquivTypes :: Pos -> Type -> Type -> FreestState ()
checkEquivTypes p expected actual = do
  kenv <- getKenv
  tenv <- getTenv
  venv <- getVenv
  when (not $ equivalent kenv tenv expected actual) $
    addError p ["Couldn't match expected type", styleRed (show expected), "\n",
             "\t with actual type", styleRed (show actual)]

checkEqualEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEqualEnvs p venv1 venv2 =
  when (not $ Map.null diff)
    (addError p ["Final environment differs from initial in", styleRed $ show diff])
  where diff = Map.difference venv2 venv1

checkEquivEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p venv1 venv2 = do
  tenv <- getTenv
  let venv1' = funSigsOnly tenv venv1
      venv2' = funSigsOnly tenv venv2
  equiv <- equivalentEnvs venv1' venv2'
  when (not equiv) $
    addError p ["Expecting environment", styleRed $ show venv1', "\n",
             "\t to be equivalent to  ", styleRed $ show venv2']

funSigsOnly :: TypeEnv -> VarEnv -> VarEnv
funSigsOnly tenv =
  Map.filterWithKey (\x _ -> not (isBuiltin x) && not (isDatatypeContructor tenv x))

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: TypeEnv -> PBind -> Bool
isDatatypeContructor tenv c =
  not $ Map.null $ Map.filter (\(_, (TypeScheme _ _ t)) -> isDatatype t) tenv
  where isDatatype :: Type -> Bool
        isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False

equivalentEnvs :: VarEnv -> VarEnv -> FreestState Bool
equivalentEnvs m1 m2 = do
  tenv <- getTenv
  return (Map.size m1 == Map.size m2) <&&>
    Map.foldlWithKey (\b x s ->
      b <&&> return (x `Map.member` m2) <&&> return (equivalentTS tenv s (m2 Map.! x))) (return True) m1

-- At parsing time all lambda variables in function definitions are
-- associated to type Unit. Here we amend the situation by replacing
-- these types with those declared in the type scheme for the
-- function.
fillFunType :: PBind -> Expression -> TypeScheme -> FreestState Type
fillFunType b@(PBind p f) e (TypeScheme _ _ t) = fill e t -- TODO: move type scheme bindings to kenv
  where
  fill :: Expression -> Type -> FreestState Type
  fill (Lambda _ _ b _ e) (Fun _ _ t1 t2) = do
    addToVenv b (toTypeScheme t1)
    t3 <- fill e t2
    removeFromVenv b
    return t3
  fill e@(Lambda p _ _ _ _) t = do
    addError p ["Couldn't match expected type", styleRed $ show t, "\n",
                "\t The equation for", styleRed f, "has one or more arguments,\n",
                "\t but its type", styleRed $ show t, "has none"]
    return t
  fill e _ = synthetise e
