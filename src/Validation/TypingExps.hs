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
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Programs
import           Syntax.Expression
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

-- | Typing rules for expressions

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
  checkUn (TypeScheme p [] t)
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
synthetise (Match p e m) = do
  t <- synthetise e
  tm <- extractInChoiceMap p t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (\k (p, e) acc ->
                                   checkMap acc venv tm k ([p], e)) (return ([],[])) m
  mapM_ (checkEquivTypes t) ts
  mapM_ (checkEquivEnvs p v) vs
  setVenv v
  return t
-- Datatype elimination
synthetise (Case p e m) = do
  t <- synthetise e
  tm <- extractDataTypeMap t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (\k v acc ->
                                   checkMap acc venv tm k v) (return ([],[])) m
  mapM_ (checkEquivTypes t) ts
  mapM_ (checkEquivEnvs p v) vs
  setVenv v
  return t

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
  checkEquivTypes t u

-- | Check an expression against a given type scheme
checkAgainstST :: Expression -> TypeScheme -> FreestState ()
checkAgainstST e (TypeScheme _ bs t) = do
  mapM_ (\(TBindK p x k) -> addToKenv (TBind p x) k) bs
  checkAgainst e t
  
-- | Check whether two given types are equivalent
checkEquivTypes :: Type -> Type -> FreestState ()
checkEquivTypes expected actual = do
  kenv <- getKenv
  tenv <- getTenv
  when (not $ equivalent kenv tenv expected actual) $
    addError (position expected) ["Couldn't match expected type", styleRed (show expected),
                                  "with actual type", styleRed (show actual)]

-- | Returns the type scheme for a variable; removes it from venv if lin
synthetiseVar :: Pos -> PVar -> FreestState TypeScheme
synthetiseVar p x = do
  let b = PBind p x
  getFromVenv b >>= \case
    Just ts -> do
      removeIfLin b ts
      return ts
    Nothing -> do
      addError p ["Variable or data constructor not in scope:", styleRed x]
      let ts = defaultTypeScheme p
      addToVenv b ts
      return ts

-- | Adds a list of binds to KindEnv
addBindsToKenv :: Pos -> [TBindK] -> FreestState ()
addBindsToKenv p = mapM_ (\(TBindK _ b k) -> addToKenv (TBind p b) k)

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

-- | Remove a variable from venv if it is linear
removeIfLin :: PBind -> TypeScheme -> FreestState ()
removeIfLin x ts = do
  k <- K.synthetiseTS ts
  when (isLin k) $ removeFromVenv x

-- Checks either the case map and the match map (all the expressions)
checkMap :: FreestState ([Type], [VarEnv]) -> VarEnv -> TypeMap -> PBind ->
            ([PBind], Expression) -> FreestState ([Type], [VarEnv])
checkMap acc venv tm b (p, e) = do
  setVenv venv
  t <- checkCons b tm -- TODO: change Bind
  mapM_ (uncurry addToVenv) (zip p (init' $ toList $ TypeScheme (position t) [] t))
  t <- synthetise e
  venv <- getVenv
  liftM (concatPair t venv) acc
  where
    init' :: [a] -> [a]
    init' [x] = [x]
    init' xs  = init xs
    concatPair :: a -> b -> ([a],[b]) -> ([a],[b])
    concatPair x y (xs, ys) = (x:xs, y:ys)

-- Checks whether a constructor exists in a map
checkCons :: PBind -> TypeMap -> FreestState Type
checkCons b@(PBind p c) tm = do
  case tm Map.!? b of
    Just x  -> return x
    Nothing -> do
      addError p ["Constructor", styleRed c, "not in scope"]
      return $ Skip p

-- | Equivalence functions

checkEqualEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEqualEnvs p venv1 venv2 =
  when (not $ Map.null diff)
    (addError p ["Final environment differs from initial in", styleRed $ show diff])
  where diff = Map.difference venv2 venv1

checkEquivEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p venv1 venv2 = do
  equiv <- equivalentEnvs venv1 venv2
  when (not equiv) $ do
    tenv <- getTenv
    addError p ["Expecting environment", styleRed $ show $ funsOnly venv1, "\n",
             "\t to be equivalent to  ", styleRed $ show $ funsOnly venv2]

funsOnly :: VarEnv -> VarEnv
funsOnly = Map.filterWithKey (\x _ -> not (isBuiltin x))

equivalentEnvs :: VarEnv -> VarEnv -> FreestState Bool
equivalentEnvs m1 m2 = do
  tenv <- getTenv
  return (Map.size m1 == Map.size m2) <&&>
    Map.foldlWithKey (\b x s ->
      b <&&> return (x `Map.member` m2) <&&> return (equivalentTS tenv s (m2 Map.! x))) (return True) m1
