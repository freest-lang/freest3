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
(
  typeCheck  
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Trans
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Validation.Kinding
import           Validation.TypeEquivalence
import           Validation.TypingState


mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trans.sequence (Map.mapWithKey (f) m)

typeCheck ::  ExpEnv -> ConstructorEnv -> TypingState ()
typeCheck eenv cenv = do
  -- 1 - Data declaration
  checkDataDecl cenv
  
  -- 2 - Function type declaration

  --let l = Map.keys eenv
  --mapM_ checkFunTypeDecl l
  mapWithKeyM (\fun _ -> checkFunTypeDecl fun) eenv
  
  -- 3 - Function declaration  
  checkFun (0,0) "start"

  venv1 <- getVarEnv
  let venv2 = Map.union venv1 cenv
  setVEnv venv2

  let a = Map.mapWithKey (\fun (a, e) -> checkFD fun a e) eenv 
  s <- get
  addErrorList $ Map.foldl (\acc v -> acc ++ errors s v) [] a
  
  return ()

errors :: (KindEnv, VarEnv, Errors) -> TypingState () -> [String]
errors is s = let (_,_,err) = execState s is in err

checkFD ::  TermVar -> Params -> Expression -> TypingState ()
checkFD fname p exp = do

  checkExpEnv (0,0) fname p
  venv <- getVarEnv
  let lt = last $ toList $ venv Map.! fname
  checkAgainst (0,0) exp lt
  -- TODO: add...
  checkVEnvUn 
  return ()

-- TODO: Add Pos
-- TODO: Test
checkVEnvUn :: TypingState ()
checkVEnvUn = do
  venv <- getVarEnv
  Trans.mapM (checkUn (0,0)) venv
-- Map.foldr (\t acc -> checkUn (0,0) t) (return ()) venv
  return ()

checkExpEnv :: Pos -> TermVar -> Params -> TypingState ()
checkExpEnv p fun params = do
  checkParam fun params
  -- TODO: 
  t <- checkFun p fun
  parameters <- addToEnv p fun params (normalizeType (init (toList t))) 
  foldM (\acc (arg, t) -> addToVEnv arg t) () parameters
  return ()

-- TESTING

normalizeType :: [TypeScheme] -> [TypeScheme]
normalizeType = map normalizeType' 

normalizeType' :: TypeScheme -> TypeScheme
normalizeType' (TypeScheme bs t) = (TypeScheme binds t)
  where
     binds = foldl (\acc b -> acc ++ (tcvar b t)) [] bs
     
     tcvar b (Fun _ t1 t2) = tcvar b t1 ++ tcvar b t2
     tcvar b (Var x)
       | var b == x = [b]
       | otherwise = []       
     tcvar b (Semi t1 t2) = tcvar b t1 ++ tcvar b t2
     tcvar b (Rec _ t) = tcvar b t
     tcvar b Skip = []
     tcvar b (Message _ _) = []
     tcvar b (Basic _) = []
     tcvar b (Choice _ m) = Map.foldl (\acc t -> acc ++ (tcvar b t)) [] m
     -- DataType, basic, pair, 
     tcvar b t = error $ "INTERNAL ERROR: " ++ show b ++ " " ++ show t



-- END TESTING       normalizeType (TypeScheme (Bind {var="x", kind=(Kind Session Un)}) (Var "x"))


addToEnv :: Pos -> TypeVar -> Params -> [TypeScheme] -> TypingState [(TypeVar, TypeScheme)]
addToEnv p c ps ts 
  | length ps == length ts = return $ zip ps ts--(map canonical ts)
  | length ps > length ts = do
      addError ("Function or constructor " ++ show c ++ " is applied to too many arguments")
      return []
  | length ps < length ts = do
      addError ("Function or constructor " ++ show c ++ " is applied to too few arguments")
      return []

checkParam :: TermVar -> Params -> TypingState ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     addError ("Conflicting definitions for " ++ fun ++
           "'\n" ++ "In an equation for '" ++ fun ++ "'")
     return ()


checkDataDecl :: ConstructorEnv -> TypingState ()
checkDataDecl cenv = do
  kenv <- getKindEnv
  Map.foldl (\_ k -> checkFunctionalKind k) (return ()) kenv
  Map.foldl (\_ t -> checkKinding t) (return ()) cenv 

checkFunctionalKind :: Kind -> TypingState ()
checkFunctionalKind k
  | k >= (Kind Functional Un) = return ()
  | otherwise = addError ("Expecting a functional (TU or TL) type; found a " ++ (show k) ++ " type.")

checkFunTypeDecl :: TermVar -> TypingState ()
checkFunTypeDecl fname = do  
  t <- checkFun (0,0) fname
  -- kenv <- getKindEnv
--  checkKinding t
  kinding t
  return ()

-- TODO: review
checkKinding :: TypeScheme -> TypingState ()
checkKinding t = do
  kinding t
  return ()


checkFun :: Pos -> TermVar -> TypingState TypeScheme
checkFun pos x = do
  member <- venvMember x
  if member then do
    (TypeScheme bs t) <- getFromVarEnv x
    return (TypeScheme bs t)
  else do
    addError (show pos ++  ": Function not in scope: " ++ x)
    return $ TypeScheme [] (Basic UnitType)

-- Typing rules for expressions

checkExp :: Expression -> TypingState TypeScheme
-- Basic expressions
checkExp (Unit _)         = return $ TypeScheme [] (Basic UnitType)
checkExp (Integer _ _)    = return $ TypeScheme [] (Basic IntType)
checkExp (Character _ _)  = return $ TypeScheme [] (Basic CharType)
checkExp (Boolean _ _)    = return $ TypeScheme [] (Basic BoolType)

-- Variables
checkExp (Variable p x) = checkVar p x
  
checkExp (UnLet p x e1 e2) = do
  t1 <- checkExp e1
  addToVEnv x t1
  t2 <- checkExp e2
  -- NEW... NEED?
  -- TODO: must check un
  removeFromVarEnv x
  return t2
  
-- Applications
checkExp (App p e1 e2) = do
  t <- checkExp e1
  (u1, u2) <- extractFun p t
  -- checkPoly p u1
  checkAgainst p e2 u1
  return u2

checkExp (TypeApp p e ts) = do
  t1 <- checkExp e  
  (binds, t2) <- extractScheme p t1
  wellFormedCall p e ts binds

  -- venv <- getVarEnv
  -- addError $ "\n\n" ++ (show venv) ++ "\n\n"
    
  -- TODO: move to other module and call subL
  let sub = foldr (\(t', b) acc -> subs t' (var b) acc) t2 (zip ts binds)
  kenv <- getKindEnv
  
  -- TODO: the result type is well formed
  
  return $ TypeScheme [] sub

-- Conditional

checkExp (Conditional p e1 e2 e3) = do
  checkAgainst p e1 (TypeScheme [] (Basic BoolType))
  venv2 <- getVarEnv
  t2 <- checkExp e2  
  venv3 <- getVarEnv
  setVEnv venv2
  checkAgainst p e3 t2
  venv4 <- getVarEnv
  kenv <- getKindEnv
  checkEquivEnvs kenv venv3 venv4
  setVEnv venv2
  return t2

 -- Pairs

checkExp (Pair p e1 e2) = do
 (TypeScheme bs1 t1) <- checkExp e1 
 (TypeScheme bs2 t2) <- checkExp e2 
 return $ TypeScheme (bs1++bs2) (PairType t1 t2)

checkExp (BinLet p x y e1 e2) = do
  t1 <- checkExp e1
  (u1,u2) <- extractPair p t1                     
  addToVEnv x u1
  addToVEnv y u2
  u <- checkExp e2

  venv <- getVarEnv
  checkUnVar venv p x 
  checkUnVar venv p y


  removeFromVarEnv x
  removeFromVarEnv y
  return u

-- Session types

checkExp (New p t) = do
  u <- checkAgainstKind t (Kind Session Lin)
  m <- extractChoiceMap u
  Map.foldrWithKey (\c t _ -> addToVEnv c (TypeScheme [] t)) (return ()) m
  return $ TypeScheme [] (PairType u (dual u))

checkExp (Send p e1 e2) = do
  venv <- getVarEnv
  t1 <- checkExp e1
  b1 <- extractBasic p t1
  t2 <- checkExp e2
  (b2, u) <- extractOutput p t2
  checkEquivBasics p b1 b2
  return u

checkExp (Receive p e) = do
  venv <- getVarEnv
  t <- checkExp e
  (b, TypeScheme bs t1) <- extractInput p t
  return $ TypeScheme bs (PairType (Basic b) t1)

-- Branching
checkExp (Select p c e) = do
  t <- checkExp e
  (TypeScheme bs choice) <- extractInChoice p t
  u <- extractConstructor p c choice  
  return $ TypeScheme bs u

checkExp (Match p e cm) = do
  t1 <- checkExp e
  let (c, (x, e1)) = Map.elemAt 0 cm
  t2 <- extractExtChoice p t1 c
  addToVEnv x t2
  venv <- getVarEnv
  u <- checkExp e1
  Map.foldrWithKey (\k (v1,v2) _ -> checkMap p t1 u k ([v1], v2) extractExtChoice)
                   (return ()) (Map.delete c cm)
  return u



checkExp (Constructor p c) = checkVar p c

checkExp (Fork p e) = do
  t <- checkExp e
  checkUn p t
  return $ TypeScheme [] (Basic UnitType)
  
checkExp (Case pos e cm) = do
  t <- checkExp e

  let (c, (x, e1)) = Map.elemAt 0 cm  
  t2 <- extractDatatype pos t c  

  let x' = zip x (init (toList t2))        
  foldM (\_ (p, t) -> addToVEnv p t) () x'  
  u <- checkExp e1
  Map.foldrWithKey (\k v _ -> checkMap pos t u k v extractDatatype)
                   (return ()) (Map.delete c cm)
  return u


  -- x is well formed (e[x] based on the kind)
wellFormedCall :: Pos -> Expression -> [Type] -> [Bind] -> TypingState ()
wellFormedCall p e ts binds = do
  kenv <- getKindEnv
  mapM (f kenv) ts
  sameNumber
  where
    f kenv t
      | isWellKinded kenv t = return ()
      | otherwise           = addError $ (show p) ++ ": Type " ++ (show t) ++ " is not well formed"
    sameNumber
      | length binds == length ts = return ()
      | otherwise                 =
        addError $ (show p) ++ ": Expecting " ++ (show (length ts)) ++
          " types on type app; found " ++ (show (length binds))


checkUnVar :: VarEnv -> Pos -> TermVar -> TypingState ()
checkUnVar venv p x
  | Map.member x venv = checkUn p (venv Map.! x)
  | otherwise         = return ()

  -- checkUn p (venv Map.! y)
  
-- checkPoly p (TypeScheme [] _) = return ()
-- checkPoly p _ = addError $ show p ++ " Not enough arguments to polymorphic function call"


checkMap :: Pos -> TypeScheme -> TypeScheme -> TermVar ->
            (Params, Expression) ->
            (Pos -> TypeScheme -> Constructor -> TypingState TypeScheme) ->
            TypingState ()
            
checkMap pos choice against c (x, e) f = do 
  t1 <- f pos choice c  
  let x' = zip x (myInit (toList t1))
  foldM (\_ (p, t) -> addToVEnv p t) () x' 
  checkAgainst pos e against
  return ()

myInit :: [a] -> [a]
myInit (x:[]) = [x]
myInit x = init x

  
-- Functions to deal with environments

checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState ()
checkEquivEnvs kenv venv1 venv2 -- = return ()
  | equivalentEnvs kenv venv1 venv2  = return ()
  | otherwise = addError ("Expecting environment " ++ show venv1 ++
                       " to be equivalent to environment " ++ show venv2)

equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
equivalentEnvs kenv venv1 venv2 =
  let venv3 = Map.filter f1 venv1
      venv4 = Map.filter f1 venv2 in
  Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
  where
    f (TypeScheme _ t) (TypeScheme _ u) = equivalent kenv t u
    f1 (TypeScheme _ t) = lin kenv t

checkEquivBasics :: Pos -> BasicType -> BasicType -> TypingState ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise = addError (show p ++ ": Expecting basic type " ++ (show b1) ++
                      " to be equivalent to basic type " ++ (show b2))

-- Some check funs

checkUn :: Pos -> TypeScheme -> TypingState ()
checkUn p (TypeScheme _ t) = do
  kenv <- getKindEnv
  if un kenv t then    
    return ()
  else
    addError (show p ++ ": Type " ++ show t ++ " is not unrestricted")



-- The Extract Functions

extractFun :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
-- extractFun _ (TypeScheme bs (Fun _ t u)) = do
--   addBindsToKenv bs
--   return (TypeScheme bs t, TypeScheme b u)
    
-- extractFun p t           = do
--   addError (show p ++  ": Expecting a function type; found " ++ show t)
--   return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))

extractFun _ (TypeScheme [] (Fun _ t u)) = do
--  addBindsToKenv bs
  return (TypeScheme [] t, TypeScheme [] u)

extractFun p (TypeScheme [] t)           = do
  addError (show p ++  ": Expecting a function type; found " ++ show t)
  return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))

extractFun p (TypeScheme bs _)           = do
--  addError $ show p ++ ": Not enough arguments to polymorphic function call"
  addError $ show p ++ "Polymorphic functions cannot be applied; instantiate function prior to applying"
  return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))


extractScheme :: Pos -> TypeScheme -> TypingState ([Bind], Type)
extractScheme p (TypeScheme [] t) = do
  addError (show p ++ ": Expecting a type scheme; found " ++ show t)
  return ([], (Basic UnitType))
extractScheme _ (TypeScheme bs t) = return (bs, t)

extractPair :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
extractPair _ (TypeScheme bs (PairType t u)) = do
  addBindsToKenv bs
  return (TypeScheme bs t, TypeScheme bs u)
extractPair p t                         = do
  addError (show p ++  ": Expecting a pair type; found " ++ show t)
  return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))

extractBasic :: Pos -> TypeScheme -> TypingState BasicType
extractBasic _ (TypeScheme bs (Basic t)) = do
  addBindsToKenv bs
  return t
extractBasic p t                         = do
  addError (show p ++  ": Expecting a basic type; found " ++ show t)
  return UnitType

-- !~>
-- TODO: review this case (bindings)
extractOutput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractOutput p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv bs
  extractOutput p (TypeScheme bs t)
extractOutput _ (TypeScheme bs (Semi (Message Out b) t)) = return (b, TypeScheme bs t)
extractOutput _ (TypeScheme bs (Message Out b)) = return (b, TypeScheme bs Skip)
extractOutput p (TypeScheme bs (Rec b t)) = do
  extractOutput p (TypeScheme bs (unfold (Rec b t)))
extractOutput p (TypeScheme bs (Semi t u)) = do -- TODO: Wrong?
  (b, TypeScheme bs' t1) <- extractOutput p (TypeScheme bs t)
  return (b, TypeScheme bs' (t1 `Semi` u))
extractOutput p t = do
  addError (show p ++  ": Expecting an output type; found " ++ show t)
  return (UnitType, TypeScheme [] Skip)

-- TODO: review this case (bindings)
extractInput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractInput p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv bs
  extractInput p (TypeScheme bs t)
extractInput _ (TypeScheme bs (Semi (Message In b) t)) =
  return (b, TypeScheme bs t)
extractInput _ (TypeScheme bs (Message In b)) = return (b, TypeScheme [] Skip)
extractInput p (TypeScheme bs (Rec b t)) = do
  addBindsToKenv bs
  extractInput p (TypeScheme bs (unfold (Rec b t)))
extractInput p (TypeScheme bs (Semi t u)) = do -- TODO: Wrong?
  (b, TypeScheme _ t1) <- extractInput p (TypeScheme bs t)
  return (b, TypeScheme bs (t1 `Semi` u))
extractInput p t = do
  addError (show p ++  ": Expecting an input type; found " ++ show t)
  return (UnitType, TypeScheme [] Skip)

-- TODO: review this case (bindings)
extractInChoice :: Pos -> TypeScheme -> TypingState TypeScheme
extractInChoice p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv bs
  extractInChoice p (TypeScheme bs t)
extractInChoice _ (TypeScheme bs (Choice Internal m)) = return $ TypeScheme bs (Choice Internal m)
extractInChoice _ (TypeScheme bs (Semi (Choice Internal m) t)) =
  return $ TypeScheme bs (Choice Internal (Map.map (`Semi` t) m))
extractInChoice p (TypeScheme bs (Rec b t)) = do
  addBindsToKenv bs
  extractInChoice p (TypeScheme bs (unfold (Rec b t)))
extractInChoice p (TypeScheme bs (Semi (Semi t1 t2) t3)) = do
  -- addBindsToKenv bs    
  (TypeScheme _ t4) <- extractInChoice p (TypeScheme bs (Semi t1 t2))
  extractInChoice p (TypeScheme bs (Semi t4 t3))  
extractInChoice p (TypeScheme bs (Semi t1 t2)) = do
  (TypeScheme bs' t3) <- extractInChoice p (TypeScheme bs t1)
  extractInChoice p (TypeScheme bs (Semi t3 t2))

  
-- extractInChoice p (TypeScheme bs (Semi t1 t2)) = do
--   -- addBindsToKenv bs
--   (TypeScheme bs' (Choice v m)) <- extractInChoice p (TypeScheme bs t1)
--   return $ TypeScheme bs (Choice v (Map.map (`Semi` t2) m))  

--(rec y . +{A:!Int;y});x


extractInChoice p t = do
  addError (show p ++  ": Expecting an internal choice; found " ++ show t)
  return $ TypeScheme [] Skip 


-- TODO: review this case (bindings)
extractExtChoice :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractExtChoice p (TypeScheme bs (Semi Skip t)) c = extractExtChoice p (TypeScheme bs t) c
extractExtChoice _ (TypeScheme bs (Choice External m)) c =
  return $ TypeScheme bs (m Map.! c) -- Choice External m
extractExtChoice _ (TypeScheme bs (Semi (Choice External m) t)) c =
  return $ TypeScheme bs ((Map.map (`Semi` t) m) Map.! c)
extractExtChoice p (TypeScheme bs (Rec b t)) c = do
  addBindsToKenv bs  
  extractExtChoice p (TypeScheme bs (unfold (Rec b t))) c
--  return b1
extractExtChoice p (TypeScheme bs (Semi t1 t2)) c = do  
  addBindsToKenv bs  
  (TypeScheme _ t3) <- extractExtChoice p (TypeScheme bs t1) c
  return $ TypeScheme bs (Semi t3 t2)  
extractExtChoice p t _ = do
  addError (show p ++  ": Expecting an external choice; found " ++ show t)
  return $ TypeScheme [] Skip

-- TODO: review this case (bindings)
extractDatatype :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractDatatype _ (TypeScheme bs (Datatype m)) c = do
  addBindsToKenv bs
  return $ TypeScheme bs (m Map.! c)
extractDatatype p (TypeScheme bs (Rec b t)) c =
  extractDatatype p (TypeScheme bs (unfold (Rec b t))) c
extractDatatype p (TypeScheme _ (Var x)) c = do -- Should be here?
  b <- venvMember x
  if b then do
    dt <- getFromVarEnv x
    extractDatatype p dt c
  else do
    addError (show p ++  ": Expecting a datatype; found " ++ show (Var x))
    return $ TypeScheme [] (Basic IntType)  
-- TODO ??
-- extractDatatype p (TypeScheme [] (Semi t1 t2)) = do
extractDatatype p t _ = do
  addError (show p ++  ": Expecting a datatype; found " ++ show t)
  return $ TypeScheme [] (Basic IntType)  

extractConstructor :: Pos -> Constructor -> Type -> TypingState Type
extractConstructor p c (Choice _ tm) =
  if Map.member c tm then
    return $ tm Map.! c 
  else do
    addError (show p ++ ": Constructor " ++ show c ++ " not in scope" )
    return (Basic UnitType)
extractConstructor p c t = do
  addError $ show p ++  ": Expecting a choice; found " ++ show t
  return (Basic UnitType)
  
-- Extract Without Errors

extractChoiceMap :: Type -> TypingState TypeMap
extractChoiceMap (Choice _ m) = return m
extractChoiceMap (Semi (Choice _ m) t2) = return (Map.map (`Semi` t2) m)
extractChoiceMap (Semi t1 t2) = do
  m1 <- extractChoiceMap t1
  m2 <- extractChoiceMap t2
  return $ Map.union m1 m2
extractChoiceMap  _ = return Map.empty


-- Checking variables

checkVar :: Pos -> TermVar -> TypingState TypeScheme
checkVar pos x = do
  member <- venvMember x
  if member then do
    (TypeScheme bs t) <- getFromVarEnv x
    addBindsToKenv bs
    kenv <- getKindEnv
    removeLinVar kenv x (TypeScheme bs t)
    return (TypeScheme bs t)
  else do
    addError (show pos ++  ": Variable or data constructor not in scope: " ++ x)
    return $ TypeScheme [] (Basic UnitType)

removeLinVar :: KindEnv -> TermVar -> TypeScheme -> TypingState ()
removeLinVar kenv x (TypeScheme _ t)
  | lin kenv t = removeFromVarEnv x   
  | otherwise  = return ()


addBindsToKenv :: [Bind] -> TypingState ()
addBindsToKenv bs = foldM (\_ b -> addToKenv (var b) (kind b)) () bs
    
-- Type check against type

checkAgainst :: Pos -> Expression -> TypeScheme -> TypingState ()
checkAgainst p e (TypeScheme bs1 t) = do
  (TypeScheme bs2 u) <- checkExp e
  kenv <- getKindEnv
  if (equivalent kenv t u) then
    return ()
  else
    addError (show p ++ ": Expecting type " ++ show u ++ " to be equivalent to type " ++ show t)
