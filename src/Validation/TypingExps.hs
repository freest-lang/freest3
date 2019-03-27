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

{-# LANGUAGE LambdaCase #-}
module Validation.TypingExps
( synthetize
, checkAgainst
, checkUn
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
import           Control.Monad.State
import qualified Data.Map.Strict as Map


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
  
synthetize (Constructor p c) = checkVar p c >>= \(TypeScheme _ _ t) -> return t

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
 
-- | The Extract Functions

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun (Fun _ _ t u) = return (t, u)
extractFun t           = do
  let p = position t
  addError p ["Expecting a function type; found:", styleRed $ show t]
  return (Basic p UnitType, Basic p UnitType)
-- extractFun p (TypeScheme bs _)           = do
--   addError p ["Polymorphic functions cannot be applied; instantiate function prior to applying"]
--   return (TypeScheme [] (Basic p UnitType), TypeScheme [] (Basic p UnitType))

-- extractFun :: Pos -> TypeScheme -> FreestState (TypeScheme, TypeScheme)
-- extractFun _ (TypeScheme p [] (Fun _ _ t u)) = return (TypeScheme p [] t, TypeScheme p [] u)
-- extractFun p (TypeScheme _ [] t)           = do
--   addError p ["Expecting a function type; found:", styleRed $ show t]
--   return (TypeScheme p [] (Basic p UnitType), TypeScheme p [] (Basic p UnitType))
-- extractFun p (TypeScheme _ bs _)           = do
--   addError p ["Polymorphic functions cannot be applied; instantiate function prior to applying"]
--   return (TypeScheme p [] (Basic p UnitType), TypeScheme p [] (Basic p UnitType))

-- Extracts a typescheme; gives an error if it is on Ɐ ε ⇒ T form
extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme p [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme p bs t) = return (bs, t)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair (PairType _ t u) = do
  return (t, u)
extractPair t                         = do
  let p = position t
  addError p ["Expecting a pair type; found ", styleRed $ show t]
  return (Basic p IntType, Basic p IntType)

-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic (Basic _ t) = return t
extractBasic t                         = do
  addError (position t) ["Expecting a basic type; found", styleRed $ show t]
  return UnitType

-- Extracts an output type from a general type; gives an error if it isn't an output
extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput (Semi _ (Skip _) t) = extractOutput t
extractOutput (Semi _ (Message _ Out b) t) = return (b, t)
extractOutput (Message p Out b) = return (b, Skip p)
extractOutput (Rec p1 b t) = extractOutput (unfold (Rec p1 b t))
extractOutput (Semi p t u) = do
  (b, t1) <- extractOutput t
  return (b, Semi p t1 u)
extractOutput t = do
  addError (position t) ["Expecting an output type; found", styleRed $ show t]
  return (UnitType, Skip (position t))

-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput (Semi _ (Skip _) t) = extractInput t
extractInput (Semi _ (Message _ In b) t) = return (b, t)
extractInput (Message p In b) = return (b, Skip p)
extractInput r@(Rec _ _ _) = extractInput (unfold r)
extractInput (Semi p t u) = do
  (b, t1) <- extractInput t
  return (b, Semi p t1 u)
extractInput t = do
  addError (position t) ["Expecting an input type; found", styleRed $ show t]
  return (UnitType, Skip (position t))

-- Extracts an internal choice from a type; gives an error if it 
extractOutChoice :: Type -> FreestState Type
extractOutChoice (Semi _ (Skip _) t) = extractOutChoice t
extractOutChoice c@(Choice _ Internal _) = return c
extractOutChoice (Semi p (Choice p1 Internal m) t) =
  return $ Choice p1 Internal (Map.map (\t1 -> Semi (position t1) t1 t) m)
extractOutChoice r@(Rec _ _ _) =  extractOutChoice (unfold r)
extractOutChoice (Semi _ (Semi p t1 t2) t3) = do
  t4 <- extractOutChoice (Semi p t1 t2)
  extractOutChoice (Semi p t4 t3)
extractOutChoice (Semi p t1 t2) = do
  t3 <- extractOutChoice t1
  extractOutChoice (Semi p t3 t2)
extractOutChoice t = do
  addError (position t) ["Expecting an internal choice; found", styleRed $ show t]
  return $ Skip (position t)

-- Extracts a constructor from a choice map;
-- gives an error if the constructor is not found
extractCons :: Pos -> Constructor -> Type -> FreestState Type
extractCons p c (Choice _ _ tm) =
  let b = Bind p c in
  case tm Map.!? b of
    Just t -> return t
    Nothing -> do
      addError p ["Constructor", styleRed $ "'"++c++"'", "not in scope"]             
      return (Basic p UnitType)
extractCons p c t = do
  addError p ["Expecting a choice; found", styleRed $ show t]
  return (Basic p UnitType)

-- Extracts an external choice map from a type;
-- gives an error if an external choice is not found
extractEChoiceMap :: Type -> FreestState TypeMap
extractEChoiceMap (Semi _ (Skip _) t) = extractEChoiceMap t
extractEChoiceMap (Choice _ External m) = return m
extractEChoiceMap (Semi _ (Choice p External m) t) =
  return $ Map.map (\t1 -> Semi p t1 t) m
extractEChoiceMap r@(Rec _ _ _) = extractEChoiceMap (unfold r)
extractEChoiceMap (Semi p t1 t2) = do
  t3 <- extractEChoiceMap t1
  return $ Map.map (\t -> Semi p t t2) t3
extractEChoiceMap t = do
  addError (position t) ["Expecting an external choice; found", styleRed $ show t]    
  return $ Map.empty

-- Extracts a datatype from a type;
-- gives an error if an external choice is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap (Datatype _ m) = return m
extractDataTypeMap t@(Var px x) = do
  getFromVenv (Bind px x) >>= \case
    Just (TypeScheme _ _ dt) -> extractDataTypeMap dt
    Nothing                  -> do
      addError px ["Expecting a datatype; found", styleRed $ show t]    
      return $ Map.empty
extractDataTypeMap t =  do
  addError (position t) ["Expecting a datatype; found", styleRed $ show t]    
  return $ Map.empty
-- TODO: are there other cases


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
equivalentEnvs venv1 venv2 = do
  venv3 <- Map.foldlWithKey f1 (return Map.empty) venv1
  venv4 <- Map.foldlWithKey f1 (return Map.empty) venv2
  kenv <- getKenv
  return $ Map.isSubmapOfBy (f kenv) venv3 venv4 && Map.isSubmapOfBy (f kenv) venv4 venv3
  return True
  where
    f :: KindEnv -> TypeScheme -> TypeScheme -> Bool
    f kenv (TypeScheme _ _ t) (TypeScheme _ _ u) = equivalent kenv t u
    
    f1 :: FreestState VarEnv -> Bind -> TypeScheme -> FreestState VarEnv
    f1 m k t@(TypeScheme _ _ t1) = do
      isLin <- K.lin t1
      if isLin then do
        m1 <- m        
        return $ Map.insert k t m1
      else m


