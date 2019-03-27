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
  , normalizeType -- TMP
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Utils.Errors
import           Utils.FreestState
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Control.Monad.State
import           Control.Conditional ((<&&>))
import qualified Data.Map.Strict as Map


-- | Typing rules for expressions

synthetize :: Expression -> FreestState Type
-- Basic expressions
synthetize (Unit p)         = return $ Basic p UnitType
synthetize (Integer p _)    = return $ Basic p IntType
synthetize (Character p _)  = return $ Basic p CharType
synthetize (Boolean p _)    = return $ Basic p BoolType
-- Variables
synthetize (Variable p x)   = do
  (TypeScheme _ _ t) <- checkVar p x -- should be (TypeScheme [] t) but there's no instance for control monad fail
  return t

synthetize (UnLet _ x e1 e2) = do
  t1 <- synthetize e1
--  (uncurry addToVenv x) (TypeScheme (position t1) [] t1)
  addToVenv x (TypeScheme (position t1) [] t1)
  t2 <- synthetize e2
  quotient x 
  return t2
  
-- Applications
synthetize (App _ e1 e2) = do
  t <- synthetize e1
  (u1, u2) <- extractFun t
  checkAgainst e2 u1
  return u2

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
  t2 <- synthetize e2  
  venv3 <- getVenv
  setVenv venv2
  checkAgainst e3 t2
  venv4 <- getVenv
  checkEquivEnvs venv3 venv4
  setVenv venv2 -- TODO: rule says venv3 but i'm not that sure
  return t2

-- Pairs
synthetize (Pair p {-m-} e1 e2) = do
  t1 <- synthetize e1 
  t2 <- synthetize e2
  {-
... mult fun on kinding
  k1 <- kinding t1
  k2 <- kinding t2
  multiplicity k1 == multiplicity k2 == m -- Fun that compares this
  -}
  return $ PairType p t1 t2

synthetize (BinLet _ x y e1 e2) = do
  t1 <- synthetize e1
  (u1,u2) <- extractPair t1  
  addToVenv x (TypeScheme (position u1) [] u1) -- TODO: Move this kind of things to state??
  addToVenv y (TypeScheme (position u2) [] u2)
  u <- synthetize e2
  venv <- getVenv
  quotient x
  quotient y
  return u

-- Session types
synthetize (New p t) = do
  K.checkAgainst (Kind p Session Lin) t
  return $ PairType p t (dual t)

synthetize (Send p e) = do
  t <- synthetize e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)

synthetize (Receive p e) = do
  -- TODO: as in send expression, allow receiving type instead of basic types only
  t <- synthetize e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2

-- Branching
synthetize (Select p c e) = do 
  t <- synthetize e
  choice <- extractInChoice t
--  addError p [show choice]
  s <- normalizeType choice -- TODO: check later - duplicated
  extractCons p c s -- choice
  
synthetize (Match _ e mm) = do
  t <- synthetize e
  tm <- extractEChoiceMap t
  venv <- getVenv
  (ts, vs) <- Map.foldrWithKey (\k (p, e) acc ->
                                  checkMap acc venv tm k ([p], e)) (return ([],[])) mm
  kenv <- getKenv  
  mapM_ (checkEquivTypes (head ts)) (tail ts)
  mapM_ (checkEquivEnvs (head vs)) (tail vs)
  setVenv $ head vs
  return $ head ts
  
synthetize (Constructor p c) = checkVar p c >>= \(TypeScheme _ _ t) -> return t

synthetize (Fork p e) = do
  t <- synthetize e
  checkUn t
  return $ Basic p UnitType

synthetize (Case _ e cm) = do -- SAME AS MATCH
  t <- synthetize e
  tm <- extractDataTypeMap t
  venv <- getVenv
  (ts, vs) <- Map.foldrWithKey (\k v acc ->
                                  checkMap acc venv tm k v) (return ([],[])) cm    
  kenv <- getKenv  
  mapM_ (checkEquivTypes (head ts)) (tail ts)
  mapM_ (checkEquivEnvs (head vs)) (tail vs)
  setVenv $ head vs
  return $ head ts


-- | Checks an expression against a given type
checkAgainst :: Expression -> Type -> FreestState ()
checkAgainst e t = do
  u <- synthetize e
  kenv <- getKenv
  if (equivalent kenv t u) then
    return ()
  else 
    addError (position t) ["Expecting type", styleRed (show u), 
                 "to be equivalent to type", styleRed (show t)]

-- | Checks whether two given types are equivalent
checkEquivTypes :: Type -> Type -> FreestState ()
checkEquivTypes t u = do
  kenv <- getKenv
  if (equivalent kenv t u) then
    return ()
  else
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
  if isUn then return ()
  else addError (position t) ["Type", "'" ++ styleRed (show t) ++ "'", "is linear"]
  
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

-- Normalizes a type
-- Ensures: The resultant type does not have a rec type, a variable, a dualof nor a
-- skip at the left-most position of a type
normalizeType :: Type -> FreestState Type
normalizeType t@(Rec _ _ _) = normalizeType $ unfold t
normalizeType (Dualof _ t) = normalizeType $ dual t
normalizeType (Var px x) = do
   getFromVenv (Bind px x) >>= \case
    Just (TypeScheme _ _ t) -> normalizeType t
    Nothing                 -> return $ Basic px UnitType
-- Semi with leading skips
normalizeType (Semi p (Skip _) u) = normalizeType u
-- Semi with choice or datatype maps
normalizeType (Semi _ (Choice p v m) u) =
  return $ Choice p v (Map.map (\t -> Semi (position t) t u) m)
normalizeType (Semi _ (Datatype p m) u) =
  return $ Datatype p (Map.map (\t -> Semi (position t) t u) m)
normalizeType (Semi p t@(Rec _ _ _) u) = normalizeType  (Semi p (unfold t) u)
-- General semi
normalizeType (Semi p t u) = do
 t1 <- normalizeType t
 return $ Semi p t1 u 
normalizeType t = return t

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun t = normalizeType t >>= extractFun'
  where 
    extractFun' :: Type -> FreestState (Type, Type)
    extractFun' (Fun _ _ t u) = return (t, u)
    extractFun' t           = do
      let p = position t
      addError p ["Expecting a function type; found:", styleRed $ show t]
      return (Basic p UnitType, Basic p UnitType)

-- extractFun :: Type -> FreestState (Type, Type)
-- extractFun (Fun _ _ t u) = return (t, u)
-- extractFun t           = do
--   let p = position t
--   addError p ["Expecting a function type; found:", styleRed $ show t]
--   return (Basic p UnitType, Basic p UnitType)
  
-- Extracts a typescheme; gives an error if it is on Ɐ ε ⇒ T form
extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme p [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme p bs t) = return (bs, t)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t = normalizeType t >>= extractPair'
  where
    extractPair' :: Type -> FreestState (Type, Type)
    extractPair' (PairType _ t u) = return (t, u)
    extractPair' t                = do
      let p = position t
      addError p ["Expecting a pair type; found ", styleRed $ show t]
      return (Basic p IntType, Basic p IntType)

-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t = normalizeType t >>= extractBasic'
  where 
    extractBasic' :: Type -> FreestState BasicType
    extractBasic' (Basic _ t) = return t
    extractBasic' t                         = do
      addError (position t) ["Expecting a basic type; found", styleRed $ show t]
      return UnitType

-- Extracts an output type from a general type; gives an error if it isn't an output

extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput t = normalizeType t >>= extractOutput'
  where
    extractOutput' ::  Type -> FreestState (BasicType, Type)
    extractOutput' (Message p Out b) = return (b, Skip p)
--    extractOutput' (Semi _ (Message _ Out b) t) = return (b, t)
    extractOutput' (Semi p t u) = do
      (b, t1) <- extractOutput' t
      return (b, Semi p t1 u)
    extractOutput' t = do
      addError (position t) ["Expecting an output type; found", styleRed $ show t]
      return (UnitType, Skip (position t))

-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput t = normalizeType t >>= extractInput'
  where
    extractInput' :: Type -> FreestState (BasicType, Type)
    extractInput' (Message p In b) = return (b, Skip p)
    extractInput' (Semi p t u) = do
      (b, t1) <- extractInput' t
      return (b, Semi p t1 u)
    extractInput' t = do
      addError (position t) ["Expecting an input type; found", styleRed $ show t]
      return (UnitType, Skip (position t))

-- Extracts an internal choice from a type; gives an error if it
extractInChoice :: Type -> FreestState Type
extractInChoice t = normalizeType t >>= extractInChoice'
  where
    extractInChoice' :: Type -> FreestState Type
    extractInChoice' c@(Choice _ Internal _) = return c
    extractInChoice' (Semi p t u) = do
      t1 <- extractInChoice' t
      return (Semi p t1 u)
    extractInChoice' t = do
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
extractEChoiceMap t = normalizeType t >>= extractEChoiceMap'
  where
    extractEChoiceMap' :: Type -> FreestState TypeMap
    extractEChoiceMap' (Choice _ External m) = return m
    extractEChoiceMap' (Semi p t1 t2) = do
      t3 <- extractEChoiceMap' t1
      return $ Map.map (\t -> Semi p t t2) t3
    extractEChoiceMap' t = do
      addError (position t) ["Expecting an external choice; found", styleRed $ show t]    
      return $ Map.empty

-- Extracts a datatype from a type;
-- gives an error if an external choice is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap t = normalizeType t >>= extractDataTypeMap'
  where
    extractDataTypeMap' (Datatype _ m) = return m
    extractDataTypeMap' t =  do
      addError (position t) ["Expecting a datatype; found", styleRed $ show t]    
      return $ Map.empty

{- | Verifies if x is well formed (e[x] based on the kind)
   | Checks if all x1,...,xn in e[x1,...,xn] are well kinded
   | Checks if the number of types (n) are admited by type
-}
  
  -- TODO: TEST
wellFormedCall :: Pos -> Expression -> [Type] -> [KBind] -> FreestState ()
wellFormedCall p e ts binds = do
  mapM_ (\t -> K.kinding (TypeScheme p [] t)) ts
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
