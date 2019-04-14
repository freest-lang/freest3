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

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.ProgramVariables
import           Syntax.Kinds
import           Syntax.Base
import qualified Validation.Kinding as K
import           Validation.Extract
import           Equivalence.Equivalence
import           Utils.Errors
import           Utils.FreestState
import           Utils.PreludeLoader (isBuiltin)
import           Control.Monad.State (when)
import qualified Data.Map.Strict as Map
import           Debug.Trace

-- SYNTHESISING A TYPE

synthetise :: KindEnv -> Expression -> FreestState Type
-- Basic expressions
synthetise _ (Unit p)        = return $ Basic p UnitType
synthetise _ (Integer p _)   = return $ Basic p IntType
synthetise _ (Character p _) = return $ Basic p CharType
synthetise _ (Boolean p _)   = return $ Basic p BoolType
-- Variable
synthetise kEnv (ProgVar p x) = do
  s@(TypeScheme _ bs t) <- synthetiseVar kEnv x
  when (not $ null bs) 
    (addError p ["Variable", styleRed $ show x, "of a polymorphic type used in a monomorphic context\n",
              "\t the type scheme for variable", styleRed $ show x, "is", styleRed $ show s])
  return t
synthetise kEnv (UnLet _ b e1 e2) = do
  t1 <- synthetise kEnv e1
  addToVEnv b (toTypeScheme t1)
  t2 <- synthetise kEnv e2
  quotient kEnv b
  return t2
-- Abstraction introduction
synthetise kEnv (Lambda p m b t1 e) = do
  K.synthetise kEnv t1
  vEnv1 <- getVEnv
  addToVEnv b (toTypeScheme t1)
  t2 <- synthetise kEnv e
  quotient kEnv b
  vEnv2 <- getVEnv
  when (m == Un) (checkEqualEnvs p vEnv1 vEnv2)
  return $ Fun p m t1 t2
-- Abstraction elimination
synthetise kEnv (App _ e1 e2) = do
  t <- synthetise kEnv e1
  (u1, u2) <- extractFun t
  checkAgainst kEnv e2 u1
  return u2
-- Type application
synthetise kEnv (TypeApp p x ts) = do
  (TypeScheme _ bs t) <- synthetiseVar kEnv x
  when (length ts /= length bs) 
    (addError p ["Wrong number of arguments to type application\n",
                "\t parameters:", styleRed $ show bs, "\n",
                "\t arguments: ", styleRed $ show ts])  
  let typeKinds = zip ts bs :: [(Type, TypeVarBind)]
  mapM (\(t, TypeVarBind _ _ k) -> K.checkAgainst kEnv k t) typeKinds
  return $ foldr (\(u, TypeVarBind _ x _) acc -> subs u x acc) t typeKinds
-- Boolean elimination
synthetise kEnv (Conditional p e1 e2 e3) = do
  checkAgainst kEnv e1 (Basic p BoolType)
  vEnv2 <- getVEnv
  t <- synthetise kEnv e2
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p kEnv vEnv3 vEnv4
  return t
-- Pair introduction
synthetise kEnv (Pair p e1 e2) = do
  t1 <- synthetise kEnv e1
  t2 <- synthetise kEnv e2
  return $ PairType p t1 t2
-- Pair elimination
synthetise kEnv (BinLet _ x y e1 e2) = do
  t1 <- synthetise kEnv e1
  (u1, u2) <- extractPair t1
  addToVEnv x (toTypeScheme u1)
  addToVEnv y (toTypeScheme u2)
  t2 <- synthetise kEnv e2
  quotient kEnv x
  quotient kEnv y
  return t2
-- Fork
-- synthetise kEnv (Fork p e) = do
--   t <- synthetise kEnv e
--   k <- K.synthetise kEnv t
--   when (isLin k) $ addError p
--     ["Unexpected linear expression", styleRed (show e), "in fork\n",
--      "\t expression", styleRed (show e), "is of type", styleRed (show t),
--      "of kind", styleRed (show k)]
--   return $ Basic p UnitType
-- Session types
synthetise kEnv (New p t) = do
  K.checkAgainst kEnv (Kind p Session Lin) t
  return $ PairType p t (Dualof p t)
synthetise kEnv (Send p e) = do
  t <- synthetise kEnv e
  (u1, u2) <- extractOutput t
  return (Fun p Lin (Basic p u1) u2)
synthetise kEnv (Receive p e) = do
  t <- synthetise kEnv e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2
synthetise kEnv (Select p c e) = do
  t <- synthetise kEnv e
  m <- extractOutChoiceMap p t
  extractCons p m c
synthetise kEnv (Match p e fm) = synthetiseFieldMap p kEnv e fm extractInChoiceMap paramsToVEnvMM
-- Datatype elimination
synthetise kEnv (Case p e fm) = synthetiseFieldMap p kEnv e fm extractDatatypeMap paramsToVEnvCM

-- | Returns the type scheme for a variable; removes it from vEnv if lin
synthetiseVar :: KindEnv -> ProgVar -> FreestState TypeScheme
synthetiseVar kEnv b = -- do
--  vEnv <- getVEnv
--  trace ("synthetiseVar: " ++ show b ++ ", vEnv: " ++ show vEnv) (return ())
  getFromVEnv b >>= \case
    Just s -> do
      k <- K.synthetiseTS kEnv s
      when (isLin k) $ removeFromVEnv b
      return s
    Nothing -> do
      let p = position b
      addError p ["Variable or data constructor not in scope:", styleRed $ show b]
      let s = omission p
      addToVEnv b s
      return s

synthetiseFieldMap :: Pos -> KindEnv -> Expression -> FieldMap ->
  (Pos -> Type -> FreestState TypeMap) ->
  (ProgVar -> [ProgVar] -> Type -> FreestState ()) -> FreestState Type
synthetiseFieldMap p kEnv e fm extract params = do
  t <- synthetise kEnv e
  tm <- extract p t
  vEnv <- getVEnv
  (t:ts, v:vs) <- Map.foldrWithKey (synthetiseField vEnv kEnv params tm) (return ([],[])) fm
  mapM_ (checkEquivTypes p kEnv t) ts
  mapM_ (checkEquivEnvs p kEnv v) vs
  setVEnv v
  return t

-- Checks either the case map and the match map (all the expressions)
synthetiseField :: VarEnv -> KindEnv -> (ProgVar -> [ProgVar] -> Type -> FreestState ()) ->
  TypeMap -> ProgVar -> ([ProgVar], Expression) ->
  FreestState ([Type], [VarEnv]) -> FreestState ([Type], [VarEnv])
synthetiseField vEnv1 kEnv params tm b (bs, e) state = do
  (ts, vEnvs) <- state
  setVEnv vEnv1
  t1 <- synthetiseCons b tm
  params b bs t1
  t2 <- fillFunType kEnv b e (toTypeScheme t1)
  mapM_ (quotient kEnv) bs  
  vEnv2 <- getVEnv
  return (t2:ts, vEnv2:vEnvs)

-- match map
paramsToVEnvMM :: ProgVar -> [ProgVar] -> Type -> FreestState ()
paramsToVEnvMM c bs t = do
  addToVEnv (head bs) (toTypeScheme t)
  let lbs = length bs
  when (lbs /= 1) $
    addError (position c) ["The label", styleRed (show c) , "should have 1",
                           "argument, but has been given", show lbs]  

paramsToVEnvCM :: ProgVar -> [ProgVar] -> Type -> FreestState ()
paramsToVEnvCM c bs t = do
  let ts =  zipProgVarLType bs t
  mapM_ (uncurry addToVEnv) ts
  let lbs = length bs
      lts = numArgs t
  when (lbs /= lts) $
    addError (position c) ["The constructor", styleRed (show c) , "should have",
                         show lts, "arguments, but has been given", show lbs]  

zipProgVarLType :: [ProgVar] -> Type -> [(ProgVar, TypeScheme)]
zipProgVarLType [] _ = []
zipProgVarLType (b:bs) (Fun _ _ t1 t2) = (b, toTypeScheme t1) : zipProgVarLType bs t2
zipProgVarLType (b:_) t = [(b, toTypeScheme t)] 

numArgs :: Type -> Int
numArgs (Fun _ _ _ t2) = 1 + numArgs t2
numArgs _              = 0

-- Check whether a constructor exists in a type map
synthetiseCons :: ProgVar -> TypeMap -> FreestState Type
synthetiseCons b tm =
  case tm Map.!? b of
    Just t  -> return t
    Nothing -> do
      addError (position b) ["Data constructor or choice field", styleRed $ show b, "not in scope"]
      return $ Skip (position b)

-- | The quotient operation
-- Removes a variable from the Environment and gives an error if it is linear
quotient :: KindEnv -> ProgVar -> FreestState ()
quotient kEnv b =
  getFromVEnv b >>= \case
    Just (TypeScheme _ [] t)  -> do
      k <- K.synthetise kEnv t
      removeFromVEnv b
      when (isLin k) $
        addError (position b)
          ["Program variable", styleRed $ show b, "is linear at the end of its scope\n",
           "\t variable", styleRed $ show b, "is of type", styleRed $ show t,
           "of kind", styleRed $ show k]
    Nothing ->
      return ()

-- CHECKING AGAINST A GIVEN TYPE OR TYPE SCHEME

-- | Check an expression against a given type
checkAgainst :: KindEnv -> Expression -> Type -> FreestState ()
-- Boolean elimination
checkAgainst kEnv (Conditional p e1 e2 e3) t = do
  checkAgainst kEnv e1 (Basic p BoolType)
  vEnv2 <- getVEnv
  checkAgainst kEnv e2 t
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p kEnv vEnv3 vEnv4
-- Pair elimination
checkAgainst kEnv (BinLet _ x y e1 e2) t2 = do
  t1 <- synthetise kEnv e1
  (u1, u2) <- extractPair t1
  addToVEnv x (toTypeScheme u1)
  addToVEnv y (toTypeScheme u2)
  checkAgainst kEnv e2 t2
  quotient kEnv x
  quotient kEnv y
-- TODO Match
-- checkAgainst kEnv (Match p e fm) = checkAgainstFieldMap p kEnv e fm extractInChoiceMap
-- TODO Datatype elimination
-- checkAgainst kEnv (Case p e fm) = checkAgainstFieldMap p kEnv e fm extractDatatypeMap
-- TODO Lambda elimination
-- Default
checkAgainst kEnv e t = do
  u <- synthetise kEnv e
  checkEquivTypes (position e) kEnv t u

-- | Check an expression against a given type scheme
checkAgainstTS :: Expression -> TypeScheme -> FreestState ()
checkAgainstTS e (TypeScheme _ bs t) = checkAgainst (K.fromTypeVarBinds bs) e t
  
-- EQUALITY AND EQUIVALENCE CHECKING

checkEquivTypes :: Pos -> KindEnv -> Type -> Type -> FreestState ()
checkEquivTypes p kEnv expected actual = do
  tEnv <- getTEnv
  vEnv <- getVEnv
  -- trace ("checkEquivTypes :" ++ show (funSigsOnly tEnv vEnv)) (return ())
  when (not $ equivalent tEnv kEnv expected actual) $
    addError p ["Couldn't match expected type", styleRed (show expected), "\n",
             "\t with actual type", styleRed (show actual)]

checkEqualEnvs :: Pos -> VarEnv -> VarEnv -> FreestState ()
checkEqualEnvs p vEnv1 vEnv2 =
  when (not $ Map.null diff)
    (addError p ["Final Environment differs from initial\n",
                "\t these extra entries are present in the final Environment:", styleRed $ show diff])
  where diff = Map.difference vEnv2 vEnv1

checkEquivEnvs :: Pos -> KindEnv -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p kEnv vEnv1 vEnv2 = do
  tEnv <- getTEnv
  let vEnv1' = funSigsOnly tEnv vEnv1
      vEnv2' = funSigsOnly tEnv vEnv2
  when (not (equivalent tEnv kEnv vEnv1' vEnv2')) $
    addError p ["Expecting Environment", styleRed (show vEnv1'), "\n",
             "\t to be equivalent to  ", styleRed (show vEnv2')]

funSigsOnly :: TypeEnv -> VarEnv -> VarEnv
funSigsOnly tEnv =
  Map.filterWithKey (\x _ -> not (isBuiltin x) && not (isDatatypeContructor tEnv x))

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: TypeEnv -> ProgVar -> Bool
isDatatypeContructor tEnv c =
  not $ Map.null $ Map.filter (\(_, (TypeScheme _ _ t)) -> isDatatype t) tEnv
  where isDatatype :: Type -> Bool
        isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False

-- At parsing time all lambda variables in function definitions are
-- associated to type Unit. Here we amend the situation by replacing
-- these types with those declared in the type scheme for the
-- function.
fillFunType :: KindEnv -> ProgVar -> Expression -> TypeScheme -> FreestState Type
fillFunType kEnv b e (TypeScheme _ _ t) = fill e t
  where
  fill :: Expression -> Type -> FreestState Type
  fill (Lambda _ _ b _ e) (Fun _ _ t1 t2) = do
    addToVEnv b (toTypeScheme t1)
    t3 <- fill e t2
    removeFromVEnv b
    return t3
  fill e@(Lambda p _ _ _ _) t = do
    addError (position b)
      ["Couldn't match expected type", styleRed $ show t, "\n",
       "\t The equation for", styleRed $ show b, "has one or more arguments,\n",
       "\t but its type", styleRed $ show t, "has none"]
    return t
  fill e _ = synthetise kEnv e
