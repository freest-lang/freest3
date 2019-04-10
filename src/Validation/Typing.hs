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
module Validation.Typing
( synthetise
, checkAgainst
, checkAgainstTS
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
import qualified Validation.Kinding as K
import           Equivalence.Equivalence
import           Utils.Errors
import           Utils.FreestState
import           Utils.PreludeLoader (isBuiltin)
import           Control.Monad.State (when)
import qualified Data.Map.Strict as Map
import           Validation.Extract
import           Debug.Trace

-- SYNTHESISING A TYPE

synthetise :: KindEnv -> Expression -> FreestState Type
-- Basic expressions
synthetise _ (Unit p)        = return $ Basic p UnitType
synthetise _ (Integer p _)   = return $ Basic p IntType
synthetise _ (Character p _) = return $ Basic p CharType
synthetise _ (Boolean p _)   = return $ Basic p BoolType
-- Variable
synthetise kenv (ProgVar p x) = do
  s@(TypeScheme _ bs t) <- synthetiseVar kenv (PBind p x)
  when (not $ null bs) 
    (addError p ["Variable", styleRed $ show x, "of a polymorphic type used in a monomorphic context\n",
              "\t the type scheme for variable", styleRed $ show x, "is", styleRed $ show s])
  return t
synthetise kenv (UnLet _ b e1 e2) = do
  t1 <- synthetise kenv e1
  addToVenv b (toTypeScheme t1)
  t2 <- synthetise kenv e2
  quotient kenv b
  return t2
-- Abstraction introduction
synthetise kenv e'@(Lambda p m b t1 e) = do
  K.synthetise kenv t1
  venv1 <- getVenv
  addToVenv b (toTypeScheme t1)
  t2 <- synthetise kenv e
  quotient kenv b
  venv2 <- getVenv
  when (m == Un) (checkEqualEnvs p venv1 venv2)
  return $ Fun p m t1 t2
-- Abstraction elimination
synthetise kenv (App _ e1 e2) = do
  t <- synthetise kenv e1
  (u1, u2) <- extractFun t
  checkAgainst kenv e2 u1
  return u2
-- Type application
synthetise kenv (TypeApp p x ts) = do
  (TypeScheme _ bs t) <- synthetiseVar kenv (PBind p x)
  when (length ts /= length bs) 
    (addError p ["Wrong number of arguments to type application\n",
                "\t parameters:", styleRed $ show bs, "\n",
                "\t arguments: ", styleRed $ show ts])  
  let typeKinds = zip ts bs :: [(Type, TBindK)]
  mapM (\(t, TBindK _ _ k) -> K.checkAgainst kenv k t) typeKinds
  return $ foldr (uncurry subs) t typeKinds
-- Boolean elimination
synthetise kenv (Conditional p e1 e2 e3) = do
  checkAgainst kenv e1 (Basic p BoolType)
  venv2 <- getVenv
  t <- synthetise kenv e2
  venv3 <- getVenv
  setVenv venv2
  checkAgainst kenv e3 t
  venv4 <- getVenv
  checkEquivEnvs p kenv venv3 venv4
  return t
-- Pair introduction
synthetise kenv (Pair p e1 e2) = do
  t1 <- synthetise kenv e1
  t2 <- synthetise kenv e2
  return $ PairType p t1 t2
-- Pair elimination
synthetise kenv (BinLet _ x y e1 e2) = do
  t1 <- synthetise kenv e1
  (u1, u2) <- extractPair t1
  addToVenv x (toTypeScheme u1)
  addToVenv y (toTypeScheme u2)
  t2 <- synthetise kenv e2
  quotient kenv x
  quotient kenv y
  return t2
-- Fork
-- synthetise kenv (Fork p e) = do
--   t <- synthetise kenv e
--   k <- K.synthetise kenv t
--   when (isLin k) $ addError p
--     ["Unexpected linear expression", styleRed (show e), "in fork\n",
--      "\t expression", styleRed (show e), "is of type", styleRed (show t),
--      "of kind", styleRed (show k)]
--   return $ Basic p UnitType
-- Session types
synthetise kenv (New p t) = do
  K.checkAgainst kenv (Kind p Session Lin) t
  return $ PairType p t (Dualof p t)
synthetise kenv (Send p e) = do
  t <- synthetise kenv e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)
synthetise kenv (Receive p e) = do
  t <- synthetise kenv e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2
synthetise kenv (Select p c e) = do
  t <- synthetise kenv e
  m <- extractOutChoiceMap p t
  extractCons p m c
synthetise kenv (Match p e fm) = synthetiseFieldMap p kenv e fm extractInChoiceMap paramsToVenvMM
-- Datatype elimination
synthetise kenv (Case p e fm) = synthetiseFieldMap p kenv e fm extractDatatypeMap paramsToVenvCM

-- | Returns the type scheme for a variable; removes it from venv if lin
synthetiseVar :: KindEnv -> PBind -> FreestState TypeScheme
synthetiseVar kenv b = do
  getFromVenv b >>= \case
    Just s -> do
      k <- K.synthetiseTS kenv s
      when (isLin k) $ removeFromVenv b
      return s
    Nothing -> do
      let p = position b
      addError p ["Variable or data constructor not in scope:", styleRed $ show b]
      let s = omission p
      addToVenv b s
      return s

synthetiseFieldMap :: Pos -> KindEnv -> Expression -> ExpMap ->
  (Pos -> Type -> FreestState TypeMap) ->
  (PBind -> [PBind] -> Type -> FreestState ()) -> FreestState Type
synthetiseFieldMap p kenv e fm extract params = do
  t <- synthetise kenv e
  tm <- extract p t
  venv <- getVenv
  (t:ts, v:vs) <- Map.foldrWithKey (synthetiseField venv kenv params tm) (return ([],[])) fm
  mapM_ (checkEquivTypes p kenv t) ts
  mapM_ (checkEquivEnvs p kenv v) vs
  setVenv v
  return t

-- Checks either the case map and the match map (all the expressions)
synthetiseField :: VarEnv -> KindEnv -> (PBind -> [PBind] -> Type -> FreestState ()) ->
  TypeMap -> PBind -> ([PBind], Expression) ->
  FreestState ([Type], [VarEnv]) -> FreestState ([Type], [VarEnv])
synthetiseField venv1 kenv params tm b (bs, e) state = do
  (ts, venvs) <- state
  setVenv venv1
  t1 <- synthetiseCons b tm
  params b bs t1
  t2 <- fillFunType kenv b e (toTypeScheme t1)
  mapM_ (quotient kenv) bs  
  venv2 <- getVenv
  return (t2:ts, venv2:venvs)

-- match map
paramsToVenvMM :: PBind -> [PBind] -> Type -> FreestState ()
paramsToVenvMM c bs t = do
  addToVenv (head bs) (toTypeScheme t)
  let lbs = length bs
  when (lbs /= 1) $
    addError (position c) ["The label", styleRed (show c) , "should have 1",
                           "argument, but has been given", show lbs]  

paramsToVenvCM :: PBind -> [PBind] -> Type -> FreestState ()
paramsToVenvCM c bs t = do
  let ts =  zipPBindLType bs t
  mapM_ (uncurry addToVenv) ts
  let lbs = length bs
      lts = numArgs t
  when (lbs /= lts) $
    addError (position c) ["The constructor", styleRed (show c) , "should have",
                         show lts, "arguments, but has been given", show lbs]  

zipPBindLType :: [PBind] -> Type -> [(PBind, TypeScheme)]
zipPBindLType [] _ = []
zipPBindLType (b:bs) (Fun _ _ t1 t2) = (b, toTypeScheme t1) : zipPBindLType bs t2
zipPBindLType (b:_) t = [(b, toTypeScheme t)] 

numArgs :: Type -> Int
numArgs (Fun _ _ _ t2) = 1 + numArgs t2
numArgs _              = 0

  
-- Check whether a constructor exists in a type map
synthetiseCons :: PBind -> TypeMap -> FreestState Type
synthetiseCons b@(PBind p c) tm = do
  case tm Map.!? b of
    Just t  -> return t
    Nothing -> do
      addError p ["Data constructor or choice field", styleRed $ show c, "not in scope"]
      return $ Skip p

-- | The quotient operation
-- Removes a variable from the environment and gives an error if it is linear
quotient :: KindEnv -> PBind -> FreestState ()
quotient kenv b@(PBind p x) =
  getFromVenv b >>= \case
    Just s  -> do
      let (TypeScheme _ [] t) = s
      k <- K.synthetise kenv t
      when (isLin k) $ addError p ["Program variable", styleRed $ show x, "is linear at the end of its scope\n",
                        "\t", "variable", styleRed $ show x, "is of type", styleRed $ show t,
                        "of kind", styleRed (show k)]
      removeFromVenv b
    Nothing ->
      return ()

-- CHECKING AGAINST A GIVEN TYPE OR TYPE SCHEME

-- | Check an expression against a given type
checkAgainst :: KindEnv -> Expression -> Type -> FreestState ()
-- Boolean elimination
checkAgainst kenv (Conditional p e1 e2 e3) t = do
  checkAgainst kenv e1 (Basic p BoolType)
  venv2 <- getVenv
  checkAgainst kenv e2 t
  venv3 <- getVenv
  setVenv venv2
  checkAgainst kenv e3 t
  venv4 <- getVenv
  checkEquivEnvs p kenv venv3 venv4
-- Pair elimination
checkAgainst kenv (BinLet _ x y e1 e2) t2 = do
  t1 <- synthetise kenv e1
  (u1, u2) <- extractPair t1
  addToVenv x (toTypeScheme u1)
  addToVenv y (toTypeScheme u2)
  checkAgainst kenv e2 t2
  quotient kenv x
  quotient kenv y
-- Match TODO
-- checkAgainst kenv (Match p e fm) = checkAgainstFieldMap p kenv e fm extractInChoiceMap
-- Datatype elimination TODO
-- checkAgainst kenv (Case p e fm) = checkAgainstFieldMap p kenv e fm extractDatatypeMap
-- Default
checkAgainst kenv e t = do
  u <- synthetise kenv e
  checkEquivTypes (position e) kenv t u

-- | Check an expression against a given type scheme
checkAgainstTS :: Expression -> TypeScheme -> FreestState ()
checkAgainstTS e (TypeScheme _ bs t) = checkAgainst (K.fromTBindKs bs) e t
  
-- EQUALITY AND EQUIVALENCE CHECKING

checkEquivTypes :: Pos -> KindEnv -> Type -> Type -> FreestState ()
checkEquivTypes p kenv expected actual = do
  tenv <- getTenv
  venv <- getVenv
  trace ("checkEquivTypes :" ++ show (funSigsOnly tenv venv)) (return ())
  when (not $ equivalent tenv kenv expected actual) $
    addError p ["Couldn't match expected type", styleRed (show expected), "\n",
             "\t with actual type", styleRed (show actual)]

checkEqualEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEqualEnvs p venv1 venv2 =
  when (not $ Map.null diff)
    (addError p ["Final environment differs from initial in", styleRed $ show diff])
  where diff = Map.difference venv2 venv1

checkEquivEnvs :: Pos -> KindEnv -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p kenv venv1 venv2 = do
  tenv <- getTenv
  let venv1' = funSigsOnly tenv venv1
      venv2' = funSigsOnly tenv venv2
  when (not (equivalent tenv kenv venv1' venv2')) $
    addError p ["Expecting environment", styleRed (show venv1'), "\n",
             "\t to be equivalent to  ", styleRed (show venv2')]

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

-- At parsing time all lambda variables in function definitions are
-- associated to type Unit. Here we amend the situation by replacing
-- these types with those declared in the type scheme for the
-- function.
fillFunType :: KindEnv -> PBind -> Expression -> TypeScheme -> FreestState Type
fillFunType kenv b@(PBind p f) e (TypeScheme _ _ t) = fill e t
  where
  fill :: Expression -> Type -> FreestState Type
  fill (Lambda _ _ b _ e) (Fun _ _ t1 t2) = do
    addToVenv b (toTypeScheme t1)
    t3 <- fill e t2
    removeFromVenv b
    return t3
  fill e@(Lambda p _ _ _ _) t = do
    addError p ["Couldn't match expected type", styleRed $ show t, "\n",
                "\t The equation for", styleRed $ show f, "has one or more arguments,\n",
                "\t but its type", styleRed $ show t, "has none"]
    return t
  fill e _ = synthetise kenv e
