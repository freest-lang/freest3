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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring#-}
-- TODO: remove NoMonadFailDesugaring and add an instance monad fail
module Validation.TypingExps
( synthetize
, checkAgainst
, checkUn
  , normalizeType -- TMP
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Control.Monad.State{-|
Module      :  TypingExps
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase #-}
import           Control.Conditional ((<&&>))
import qualified Data.Map.Strict as Map
import           Validation.Extract


-- | Typing rules for expressions

synthetize :: Expression -> FreestState Type
-- Basic expressions
synthetize (Unit p)         = return $ Basic p UnitType
synthetize (Integer p _)    = return $ Basic p IntType
synthetize (Character p _)  = return $ Basic p CharType
synthetize (Boolean p _)    = return $ Basic p BoolType
-- Variable
synthetize (Variable p x)   = do
  (TypeScheme _ _ t) <- checkVar p x -- should be (TypeScheme [] t) but there's no instance for control monad fail
  return t
-- Derived TODO: to be remove
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
  t' <- checkVar p x
  (bs, t) <- extractScheme t'
-- wellFormedCall p e ts binds
  let typeBind = zip ts bs
  mapM (\(t, KBind _ _ k) -> K.checkAgainst k t) typeBind
  -- well formed sub??
  return $ subL t typeBind
     
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

-- Session types
synthetize (New p t) = do
  K.checkAgainst (Kind p Session Lin) t
  return $ PairType p t (dual t) -- Could be Dualof t; perhaps better error messages

synthetize (Send p e) = do
  t <- synthetize e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)

synthetize (Receive p e) = do
  t <- synthetize e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2

-- Branching
synthetize (Select p c e) = do 
  t <- synthetize e
  choice <- extractOutChoice t -- TODO: merge these two functions
  u <- extractCons p c choice  
  return u

synthetize (Match _ e m) = do
  t <- synthetize e
  tm <- extractEChoiceMap t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (\k (p, e) acc ->
                                  checkMap acc venv tm k ([p], e)) (return ([],[])) m
  mapM_ (checkEquivTypes t) ts
  mapM_ (checkEquivEnvs v) vs
  setVenv v
  return t

  
-- synthetize (Constructor p c) = checkVar p c >>= \(TypeScheme _ _ t) -> return t

synthetize (Fork p e) = do
  t <- synthetize e
  checkUn t
  return $ Basic p UnitType

synthetize (Case _ e m) = do -- SAME AS MATCH
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
  kenv <- getKenv
  when (not $ equivalent kenv t u) $
    addError (position t) ["Expecting type", styleRed (show u), 
                           "to be equivalent to type", styleRed (show t)]

-- | Check whether two given types are equivalent
checkEquivTypes :: Type -> Type -> FreestState ()
checkEquivTypes t u = do
  kenv <- getKenv
  when (not $ equivalent kenv t u) $
    addError (position t) ["Expecting type", styleRed (show u), 
                           "to be equivalent to type", styleRed (show t)]

-- | The quotient operation
-- removes a variable from the environment and gives an error if it is linear
quotient :: Bind -> FreestState ()
quotient b = do
  getFromVenv b >>= \case -- venv Map.!? b of
    Just (TypeScheme _ _ t) -> checkUn t
    Nothing                 -> return ()
  removeFromVenv b

-- | Checks whether a type is unrestricted
checkUn :: Type -> FreestState ()
checkUn t = do
  isUn <- K.un t
  when (not isUn) $
    addError (position t) ["Expecting an unrestricted type; found", styleRed (show t)]
  
-- | Checking Variables
-- | Checks whether a variable exists and removes it if is a linear usage
checkVar :: Pos -> TermVar -> FreestState TypeScheme -- TODO: Review
checkVar p x = do
  let b = (Bind p x)
  getFromVenv b >>= \case
    Just t@(TypeScheme _ bs _) -> do
      addBindsToKenv p bs    
      removeLinVar b t
      return t
    Nothing -> do
      let t = (TypeScheme p [] (Basic p UnitType))
      addError p ["Variable or data constructor not in scope:", styleRed x]
      addToVenv b t
      return t
 
-- | Adds a list of binds to KindEnv
addBindsToKenv :: Pos -> [KBind] -> FreestState ()
addBindsToKenv p bs = foldM (\_ (KBind _ b k) -> addToKenv (Bind p b) k) () bs

-- | Removes a variable from venv if it is linear
removeLinVar :: Bind -> TypeScheme -> FreestState ()
removeLinVar x (TypeScheme _ _ t) = do
  isLin <- K.lin t
  if isLin then removeFromVenv x
  else return ()
 
{- | Verifies if x is well formed (e[x] based on the kind)
   | Checks if all x1,...,xn in e[x1,...,xn] are well kinded
   | Checks if the number of types (n) are admited by type
-}
  
  -- TODO: TEST
wellFormedCall :: Pos -> Expression -> [Type] -> [KBind] -> FreestState ()
wellFormedCall p e ts binds = do
  mapM_ (\t -> K.synthetize t) ts
  sameNumber
  where   
    sameNumber
      | length binds == length ts = return ()
      | otherwise                 =          
          addError p ["Expecting", show $ length binds,
                      "type(s) on type app; found", show $ length ts]

-- Checks either the case map and the match map (all the expressions)
checkMap :: FreestState ([Type],[VarEnv]) -> VarEnv -> TypeMap -> Bind ->
            ([Bind], Expression) -> FreestState ([Type],[VarEnv])
checkMap acc venv tm b (p, e) = do
  setVenv venv
  t <- checkCons b tm -- TODO: change Bind
  foldM (\_ (x@(Bind p _), t') -> addToVenv x t') ()
     (zip p (init' $ toList $ TypeScheme (position t) [] t))

  t <- synthetize e 
  venv <- getVenv
  liftM (concatPair t venv) acc 
  
  where
    init' :: [a] -> [a]
    init' (x:[]) = [x]
    init' x      = init x

    concatPair :: a -> b -> ([a],[b]) -> ([a],[b])
    concatPair x y (xs, ys) = (x:xs, y:ys)

-- Checks whether a constructor exists in a map
checkCons :: Bind -> TypeMap -> FreestState Type
checkCons b@(Bind p c) tm = do
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
    f :: Bind -> TypeScheme -> VarEnv -> FreestState Bool
    f b (TypeScheme _ _ t) venv = do
      lt <- K.lin t
      if lt then
        case venv Map.!? b of
          Nothing -> return False
          Just (TypeScheme _ _ u) -> do
            lu <- K.lin u
            if lu then getKenv >>= \k -> return $ equivalent k t u
            else return False
       else return True
