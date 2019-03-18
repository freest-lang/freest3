{-|
Module      :  Typing
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Typing
( typeCheck
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
import           Data.List ((\\), nub, intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Trav

typeCheck :: FreestState ()
typeCheck = do
  -- 1 - Data declaration
  -- Checks if the datatypes are well kinded
  checkDataDecl
  
  -- 2 - Function type declaration
  -- Checks if all function types are well kinded and if
  -- all the declared functions have types (function signatures)
  eenv <- getEenv
  mapWithKeyM (\fun _ -> checkFunTypeDecl fun) eenv -- TODO: Map over keys

  --  3 - Function declaration
  venv1 <- getVenv
  -- cenv <- getCenv
  -- let venv2 = Map.union venv1 cenv
  -- setVenv venv2
  
  -- TODO: added venv2 argument. Not sure if its ok
  mapWithKeyM (\fun (a, e) -> checkFD venv1 fun a e) eenv

  venv <- getVenv
  Trav.mapM (\(TypeScheme _ _ t) -> checkUn t) venv
  return ()


-- | AUXILIARY FUNCTIONS TO VERIFY DATATYPES

{- | Checks all the datatypes definitions:
   |  - Checks if they are well kinded and if they have a functional kind
-}
checkDataDecl :: FreestState ()
checkDataDecl = do 
  kenv <- getKenv
  mapM_ (\k -> checkFunctionalKind k) kenv
  cenv <- getCenv
  mapM_ K.kinding cenv



checkFunctionalKind :: Kind -> FreestState ()
checkFunctionalKind k
  | k >= (Kind (position k) Functional Un) = return ()
  | otherwise = 
     addError (position k) ["Expecting a functional (TU or TL) type; found a",
                  styleRed (show k), "type."]


-- | AUXILIARY FUNCTIONS TO VERIFY FUNCTION TYPES

-- | Verifies if a function exists and if it is well kinded
checkFunTypeDecl :: Bind -> FreestState ()
checkFunTypeDecl b = do  
  t <- checkFun b
  K.kinding t
  return ()

checkFun :: Bind -> FreestState TypeScheme
checkFun b@(Bind pos x) = do
  member <- venvMember b
  if member then do
    (TypeScheme p bs t) <- getFromVenv b
    return $ TypeScheme p bs t
  else do
    addError pos ["Function", styleRed ("'" ++ x ++ "'"), "not in scope"]
    addToVenv b (TypeScheme pos [] (Basic pos UnitType))
    return $ TypeScheme pos [] (Basic pos UnitType)


-- | FUNCTION DECLARATION

{- | Checks a function declaration:
   |  - Checks the function form
   |  - Checks the function body (expression) against the declared type
-}
checkFD ::  VarEnv -> Bind -> [Bind] -> Expression -> FreestState ()
checkFD venv fname p exp = do
  checkFunForm venv fname p
  let t = venv Map.! fname
--  let lt = last $ toList t
  let (TypeScheme _ _ lt) = last $ toList t
  checkAgainst exp lt
--  checkAgainst tp exp lt

--  venv <- getVenv
--  Trav.mapM (\(p, t) -> checkUn p t) venv
  return ()


{- | Checks the form of one function:
   | - Checks if a function is applied to the correct number of arguments
   | - Adds each argument and its own type to the environment
-}

checkFunForm :: VarEnv -> Bind -> [Bind] -> FreestState ()
checkFunForm venv fun args = do
--  checkArgsConflits fun args
  let t = venv Map.! fun
  arguments <- checkArgs fun args (normalizeType (init (toList t)))
  foldM (\acc (b@(Bind p _), t) -> addToVenv b t) () arguments
  return ()

checkArgs :: Bind -> [Bind] -> [TypeScheme] -> FreestState [(Bind, TypeScheme)]
checkArgs (Bind p c) ps ts
  | length ps == length ts = return $ zip ps ts
  | length ps > length ts = do
      addError p ["Function or constructor '", styleRed c ++ "'",
                  "is applied to too many arguments"]
      return []
  | length ps < length ts = do
      addError p ["Function or constructor '", styleRed c ++ "'",
                  "is applied to too few arguments"]
      return []


-- | TODO: check this ... Temporary... TESTING

normalizeType :: [TypeScheme] -> [TypeScheme]
normalizeType = map normalizeType' 

normalizeType' :: TypeScheme -> TypeScheme
normalizeType' (TypeScheme p bs t) = (TypeScheme p binds t)
  where
     binds = foldl (\acc b -> acc ++ (tcvar b t)) [] bs
     
     tcvar b (Fun _ _ t1 t2) = tcvar b t1 ++ tcvar b t2
     tcvar (KBind p y k) (Var _ x)
       | y == x = [KBind p x k]
       | otherwise = []       
     tcvar b (Semi _ t1 t2) = tcvar b t1 ++ tcvar b t2
     tcvar b (Rec _ _ t) = tcvar b t
     tcvar b (Skip _) = []
     tcvar b (Message _ _ _) = []
     tcvar b (Basic _ _) = []
     tcvar b (Choice _ _ m) = Map.foldl (\acc t -> acc ++ (tcvar b t)) [] m
     tcvar b (PairType _ t1 t2) = tcvar b t1 ++ tcvar b t2
     -- DataType
     tcvar b t = error $ "INTERNAL ERROR: " ++ show b ++ " " ++ show t

-- | Checks if a type is unrestricted
checkUn :: Type -> FreestState ()
checkUn t = do
  isUn <- K.un t
  if isUn then    
    return ()
  else
    addError (position t) ["Type", "'" ++ styleRed (show t) ++ "'", "is linear"]

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

checkEquivTypes :: Type -> Type -> FreestState ()
checkEquivTypes t u = do
  kenv <- getKenv
  if (equivalent kenv t u) then
    return ()
  else
    addError (position t) ["Expecting type", styleRed (show u), 
                 "to be equivalent to type", styleRed (show t)]

quotient :: Bind -> FreestState ()
quotient b = do
  venv <- getVenv
  case venv Map.!? b of
    Just (TypeScheme _ _ t) -> checkUn t
    Nothing                 -> return ()
  removeFromVenv b

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

-- -- Pairs
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
{-
synthetize (Send p e1 e2) = do
  -- TODO: This one is not aligned with the journal rules (send e e -> send e)
  -- TODO: allow sending type instead of basic types only
  t1 <- synthetize e1
  b1 <- extractBasic t1
  t2 <- synthetize e2
  (b2, u) <- extractOutput t2
  checkEquivBasics p b1 b2
  return u
-}
synthetize (Receive p e) = do
  -- TODO: as in send expression, allow receiving type instead of basic types only
  t <- synthetize e
  (u1, u2) <- extractInput t
  return $ PairType p (Basic p u1) u2

-- Branching
synthetize (Select p c e) = do 
  t <- synthetize e
  choice <- extractInChoice t
  u <- extractCons p c choice  
  return u

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
  
synthetize (Constructor p c) = do
  (TypeScheme _ _ t) <- checkVar p c -- should be (TypeScheme [] t) but there's no instance for control monad fail
  return t

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

-- | Checking Variables

{- | Checks a variable and removes it from the environment if
     it is linear.
-}
checkVar :: Pos -> TermVar -> FreestState TypeScheme -- TODO: Review
checkVar p x = do
  let b = Bind p x
  member <- venvMember b
  if member then do
    t@(TypeScheme _ bs _) <- getFromVenv b
    addBindsToKenv p bs
    kenv <- getKenv
    removeLinVar kenv b t
    return t
  else do
    addError p ["Variable or data constructor not in scope:", styleRed x]
    addToVenv (Bind p x) (TypeScheme p [] (Basic p UnitType))
    return $ TypeScheme p [] (Basic p UnitType)

addBindsToKenv :: Pos -> [KBind] -> FreestState ()
addBindsToKenv p bs = foldM (\_ (KBind _ b k) -> addToKenv (Bind p b) k) () bs

removeLinVar :: KindEnv -> Bind -> TypeScheme -> FreestState ()
removeLinVar kenv x (TypeScheme _ _ t) = do
  isLin <- K.lin t
  if isLin
    then removeFromVenv x
  else return ()
 
-- | The Extract Functions


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


extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme p [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme p bs t) = return (bs, t)

extractPair :: Type -> FreestState (Type, Type)
extractPair (PairType _ t u) = do
  return (t, u)
extractPair t                         = do
  let p = position t
  addError p ["Expecting a pair type; found ", styleRed $ show t]
  return (Basic p IntType, Basic p IntType)
  
extractBasic :: Type -> FreestState BasicType
extractBasic (Basic _ t) = return t
extractBasic t                         = do
  addError (position t) ["Expecting a basic type; found", styleRed $ show t]
  return UnitType

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


extractInChoice :: Type -> FreestState Type
extractInChoice (Semi _ (Skip _) t) = extractInChoice t
extractInChoice c@(Choice _ Internal _) = return c
extractInChoice (Semi p (Choice p1 Internal m) t) =
  return $ Choice p1 Internal (Map.map (\t1 -> Semi (position t1) t1 t) m)
extractInChoice r@(Rec _ _ _) =  extractInChoice (unfold r)
extractInChoice (Semi _ (Semi p t1 t2) t3) = do
  t4 <- extractInChoice (Semi p t1 t2)
  extractInChoice (Semi p t4 t3)
extractInChoice (Semi p t1 t2) = do
  t3 <- extractInChoice t1
  extractInChoice (Semi p t3 t2)
extractInChoice t = do
  addError (position t) ["Expecting an internal choice; found", styleRed $ show t]
  return $ Skip (position t)

extractCons :: Pos -> Constructor -> Type -> FreestState Type
extractCons p c (Choice _ _ tm) =
  let b = Bind p c in   
  if Map.member b tm then
    return $ tm Map.! b
  else do
    addError p ["Constructor", styleRed $ "'"++c++"'", "not in scope"]             
    return (Basic p UnitType)
extractCons p c t = do
  addError p ["Expecting a choice; found", styleRed $ show t]
  return (Basic p UnitType)

-- TODO: review this case (bindings)
-- TODO: error on Map.!
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

-- 
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap (Datatype _ m) = return m
extractDataTypeMap t@(Var px x) = do
  venv <- getVenv -- TODO: change to Maybe
  case venv Map.!? (Bind px x) of
    Just (TypeScheme _ _ dt) -> extractDataTypeMap dt
    Nothing                  -> do
      addError px ["Expecting a datatype; found", styleRed $ show t]    
      return $ Map.empty
extractDataTypeMap t =  do
  addError (position t) ["Expecting a datatype; found", styleRed $ show t]    
  return $ Map.empty
-- TODO: are there other cases


-- | AUXILIARY FUNCTIONS

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


-- checkMap :: FreestState ([Type],[VarEnv]) -> VarEnv -> TypeMap -> Bind ->
--             ([Bind], Expression) -> FreestState ([Type],[VarEnv])
checkMap :: FreestState ([Type],[VarEnv]) -> VarEnv -> TypeMap -> TermVar ->
            ([Bind], Expression) -> FreestState ([Type],[VarEnv])
checkMap acc venv tm x (p, e) = do
  setVenv venv
  t <- checkCons x tm
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


-- TODO: Pos    
--checkCons :: Bind -> TypeMap -> FreestState Type
checkCons :: TermVar -> TypeMap -> FreestState Type
checkCons c tm = do
  case tm Map.!? (Bind defaultPos c) of
    Just x  -> return x
    Nothing -> do
      addError defaultPos [styleRed "Rewrite, label not in scope"]
      return $ Skip defaultPos


-- | Equivalence functions


-- | TODO: position, diff, better error message, maybe with diff between maps
-- and something else (only compares keys)
checkEquivEnvs :: VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs venv1 venv2 = do
  equiv <- equivalentEnvs venv1 venv2
  if equiv then
    return ()
  else
      addError defaultPos ["Expecting environment", show venv1,
                    "to be equivalent to environment", show venv2]

 -- | TODO: position, diff, better error message, maybe with diff between maps
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

checkEquivBasics :: Pos -> BasicType -> BasicType -> FreestState ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise =
      addError p ["Expecting basic type", styleRed $ show b1,
                  "to be equivalent to basic type", styleRed $ show b2]

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)
