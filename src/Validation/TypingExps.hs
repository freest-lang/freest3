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
( synthetize
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
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Control.Monad.State
import           Control.Conditional ((<&&>))
import qualified Data.Map.Strict as Map
import           Validation.Extract
import           Debug.Trace

-- | Typing rules for expressions

synthetize :: Expression -> FreestState Type
-- Basic expressions
synthetize (Unit p)        = return $ Basic p UnitType
synthetize (Integer p _)   = return $ Basic p IntType
synthetize (Character p _) = return $ Basic p CharType
synthetize (Boolean p _)   = return $ Basic p BoolType
-- Variable
synthetize (ProgVar p x) = do
  (TypeScheme _ _ t) <- checkVar p x -- should be (TypeScheme [] t) but there's no instance for control monad fail
  return t
synthetize (UnLet _ x e1 e2) = do
  t1 <- synthetize e1
  addToVenv x (TypeScheme (position t1) [] t1)
  t2 <- synthetize e2
  quotient x
  return t2
-- Abstraction elimination
synthetize (App _ e1 e2) = do
  t <- synthetize e1
  (u1, u2) <- extractFun t
  checkAgainst e2 u1
  return u2
-- Type application
synthetize (TypeApp p x ts) = do
  s <- checkVar p x
  (bs, t) <- extractScheme s
  let typeBinds = zip ts bs
  mapM (\(t, TBindK _ _ k) -> K.checkAgainst k t) typeBinds
  return $ foldr (uncurry subs) t typeBinds
-- Conditional
synthetize (Conditional p e1 e2 e3) = do
  checkAgainst e1 (Basic p BoolType)
  venv2 <- getVenv
  t <- synthetize e2
  venv3 <- getVenv
  setVenv venv2
  checkAgainst e3 t
  venv4 <- getVenv
  checkEquivEnvs venv3 venv4
  return t
-- Pair introduction
synthetize (Pair p e1 e2) = do
  t1 <- synthetize e1
  t2 <- synthetize e2
  return $ PairType p t1 t2
-- Pair elimination
synthetize (BinLet _ x y e1 e2) = do
  t <- synthetize e1
  (u1,u2) <- extractPair t
  addToVenv x (TypeScheme (position u1) [] u1)
  addToVenv y (TypeScheme (position u2) [] u2)
  u <- synthetize e2
  venv <- getVenv
  quotient x
  quotient y
  return u
-- Fork
synthetize (Fork p e) = do
  t <- synthetize e
  checkUn (TypeScheme p [] t)
  return $ Basic p UnitType
-- Session types
synthetize (New p t) = do
  K.checkAgainst (Kind p Session Lin) t
  return $ PairType p t (Dualof p t)
synthetize (Send p e) = do
  t <- synthetize e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)
synthetize (Receive p e) = do
  t <- synthetize e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2
synthetize (Select p c e) = do
  t <- synthetize e
  m <- extractOutChoiceMap p t
  extractCons p m c
synthetize (Match p e m) = do
  t <- synthetize e
  tm <- extractInChoiceMap p t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (\k (p, e) acc ->
                                  checkMap acc venv tm k ([p], e)) (return ([],[])) m
  mapM_ (checkEquivTypes t) ts
  mapM_ (checkEquivEnvs v) vs
  setVenv v
  return t
-- Datatype elimination
synthetize (Case _ e m) = do
  t <- synthetize e
  tm <- extractDataTypeMap t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (\k v acc ->
                                  checkMap acc venv tm k v) (return ([],[])) m
  mapM_ (checkEquivTypes t) ts
  mapM_ (checkEquivEnvs v) vs
  setVenv v
  return t

-- | Check an expression against a given type
checkAgainst :: Expression -> Type -> FreestState ()
-- Conditional
checkAgainst (Conditional p e1 e2 e3) t = do
  checkAgainst e1 (Basic p BoolType)
  venv2 <- getVenv
  checkAgainst e2 t
  venv3 <- getVenv
  setVenv venv2
  checkAgainst e3 t
  venv4 <- getVenv
  checkEquivEnvs venv3 venv4
-- Default
checkAgainst e t = do
  u <- synthetize e
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

-- | Checking Variables
-- | Checks whether a variable exists and removes it if is a linear usage
checkVar :: Pos -> PVar -> FreestState TypeScheme -- TODO: Review
checkVar p x = do
  let b = PBind p x
  getFromVenv b >>= \case
    Just t@(TypeScheme _ bs _) -> do
      addBindsToKenv p bs
      removeIfLin b t
      return t
    Nothing -> do
      addError p ["Variable or data constructor not in scope:", styleRed x]
      let t = (TypeScheme p [] (Basic p UnitType))
      addToVenv b t
      return t

-- | Adds a list of binds to KindEnv
addBindsToKenv :: Pos -> [TBindK] -> FreestState ()
addBindsToKenv p = mapM_ (\(TBindK _ b k) -> addToKenv (TBind p b) k)

-- | The quotient operation
-- removes a variable from the environment and gives an error if it is linear
quotient :: PBind -> FreestState ()
quotient b = do
  getFromVenv b >>= \case
    Just t  -> checkUn t
    Nothing -> return ()
  removeFromVenv b

-- | Checks whether a type is unrestricted
checkUn :: TypeScheme -> FreestState ()
checkUn t = do
  isUn <- K.un t
  when (not isUn) $
    addError (position t) ["Expecting an unrestricted type; found", styleRed (show t)]

-- | Removes a variable from venv if it is linear
removeIfLin :: PBind -> TypeScheme -> FreestState ()
removeIfLin x t = do
  isLin <- K.lin t
  when isLin $ removeFromVenv x

{- | Verifies if x is well formed (e[x] based on the kind)
   | Checks if all x1,...,xn in e[x1,...,xn] are well kinded
   | Checks if the number of types (n) are admited by type
  -- TODO: TEST
wellFormedCall :: Pos -> Expression -> [Type] -> [TBindK] -> FreestState ()
wellFormedCall p e ts binds = do
  mapM_ (\t -> K.synthetize t) ts
  sameNumber
  where
    sameNumber
      | length binds == length ts = return ()
      | otherwise                 =
          addError p ["Expecting", show $ length binds,
                      "type(s) on type app; found", show $ length ts]
-}

-- Checks either the case map and the match map (all the expressions)
checkMap :: FreestState ([Type], [VarEnv]) -> VarEnv -> TypeMap -> PBind ->
            ([PBind], Expression) -> FreestState ([Type], [VarEnv])
checkMap acc venv tm b (p, e) = do
  setVenv venv
  t <- checkCons b tm -- TODO: change Bind
  mapM_ (uncurry addToVenv) (zip p (init' $ toList $ TypeScheme (position t) [] t))
  t <- synthetize e
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

-- TODO: position, diff, better error message, maybe with diff between maps
-- and something else (only compares keys)
checkEquivEnvs :: VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs venv1 venv2 = do
  equiv <- equivalentEnvs venv1 venv2
  if equiv then
    return ()
  else
      addError defaultPos ["Expecting environment", show venv1,
                    "to be equivalent to environment", show venv2]

 -- TODO: position, diff, better error message, maybe with diff between maps
equivalentEnvs :: VarEnv -> VarEnv -> FreestState Bool
equivalentEnvs venv1 venv2 = Map.foldrWithKey (\b t acc -> acc <&&> f b t venv2) (return True) venv1
  where
    f :: PBind -> TypeScheme -> VarEnv -> FreestState Bool
    f b t venv = do
      lt <- K.lin t
      if lt then
        case venv Map.!? b of
          Nothing -> return False
          Just u -> do
            lu <- K.lin u
            if lu
            then do
              let (TypeScheme _ _ t', TypeScheme _ _ u') = (t, u)
              kenv <- getKenv
              tenv <- getTenv
              return $ equivalent kenv tenv t' u'
            else return False
       else return True
