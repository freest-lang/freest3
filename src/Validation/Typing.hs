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

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Utils.Errors
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Validation.TypingState

import           Control.Monad.State
import           Data.List ((\\), nub, intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Trav


typeCheck :: TypingState ()
typeCheck = do
  -- 1 - Data declaration
  -- Checks if the datatypes are well kinded
  checkDataDecl
  
  -- 2 - Function type declaration
  -- Checks if all function types are well kinded and if
  -- all the declared functions have types (function signatures)
  eenv <- getEenv
  mapWithKeyM (\fun (p,_,_) -> checkFunTypeDecl p fun) eenv

  --  3 - Function declaration
  venv1 <- getVenv
  cenv <- getCenv
  let venv2 = Map.union venv1 cenv
  setVenv venv2
  
  -- TODO: added venv2 argument. Not sure if its ok
  mapWithKeyM (\fun (_, a, e) -> checkFD venv2 fun a e) eenv

  venv <- getVenv
  Trav.mapM (\(p, t) -> checkUn p t) venv
  return ()


-- | AUXILIARY FUNCTIONS TO VERIFY DATATYPES

{- | Checks all the datatypes definitions:
   |  - Checks if they are well kinded and if they have a functional kind
-}
checkDataDecl :: TypingState ()
checkDataDecl = do 
  kenv <- getKenv
  mapM_ (\k -> checkFunctionalKind k) kenv
  cenv <- getCenv
--  mapM_ (\(_,t) -> checkKinding t) cenv
  mapM_ (checkKinding . snd) cenv


checkFunctionalKind :: (Pos, Kind) -> TypingState () -- TODO: remove Pos
checkFunctionalKind (p, k)
  | k >= (Kind p Functional Un) = return ()
  | otherwise = 
     addError p ["Expecting a functional (TU or TL) type; found a",
                  styleRed (show k), "type."]

checkKinding :: TypeScheme -> TypingState ()
checkKinding t = do
  K.kinding t
  return ()

-- | AUXILIARY FUNCTIONS TO VERIFY FUNCTION TYPES

-- | Verifies if a function exists and if it is well kinded
checkFunTypeDecl :: Pos -> TermVar -> TypingState ()
checkFunTypeDecl pos fname = do  
  t <- checkFun pos fname
  K.kinding t
  return ()

checkFun :: Pos -> TermVar -> TypingState TypeScheme
checkFun pos x = do
  member <- venvMember x
  if member then do
    (_, (TypeScheme p bs t)) <- getFromVenv x
    return $ TypeScheme p bs t
  else do
    addError pos ["Function", styleRed ("'" ++ x ++ "'"), "not in scope"]
    addToVenv pos x (TypeScheme pos [] (Basic pos UnitType))
    return $ TypeScheme pos [] (Basic pos UnitType)


-- | FUNCTION DECLARATION

{- | Checks a function declaration:
   |  - Checks the function form
   |  - Checks the function body (expression) against the declared type
-}
checkFD ::  VarEnv -> TermVar -> Params -> Expression -> TypingState ()
checkFD venv fname p exp = do
  checkFunForm venv fname p
  let (tp, t) = venv Map.! fname
  let lt = last $ toList t
  checkAgainst tp exp lt

--  venv <- getVenv
--  Trav.mapM (\(p, t) -> checkUn p t) venv
  return ()


{- | Checks the form of one function:
   | - Checks the confliting definitions of arguments
   | - Checks if a function is applied to the correct number of arguments
   | - Adds each argument and its own type to the environment
-}

checkFunForm :: VarEnv -> TermVar -> Params -> TypingState ()
checkFunForm venv fun args = do
--  checkArgsConflits fun args
  let (p,t) = venv Map.! fun
  arguments <- checkArgs p fun args (normalizeType (init (toList t)))
  foldM (\acc (arg, t) -> addToVenv (paramPos arg) (param arg) t) () arguments
  return ()

-- checkArgsConflits :: TermVar -> Params -> TypingState ()
-- checkArgsConflits fun args
--   | length args == length (Set.fromList args) = return ()
--   | otherwise                                = do
--      (p,_,_) <- getFromEenv fun
--      mapM err clash
--      return ()
--   where
--     err (Param p c) = addError p ["Conflicting definitions for", showClashes c,
--                           "\n\t In an equation for", styleRed $ "'" ++ fun ++ "'"]
--     clash = args \\ nub args
--     showClashes x = styleRed $ "'" ++  x ++ "'"

checkArgs :: Pos -> TypeVar -> Params -> [TypeScheme] -> TypingState [(Param, TypeScheme)]
checkArgs p c ps ts
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
     tcvar (Bind p y k) (Var _ x)
       | y == x = [Bind p x k]
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
checkUn :: Pos -> TypeScheme -> TypingState ()
checkUn p (TypeScheme _ _ t) = do
  isUn <- K.un t
  if isUn then    
    return ()
  else
    addError p ["Type", "'" ++ styleRed (show t) ++ "'", "is linear"]

-- | Checks an expression against a given type
checkAgainst :: Pos -> Expression -> TypeScheme -> TypingState ()
checkAgainst p e (TypeScheme _ bs1 t) = do
  (TypeScheme _ bs2 u) <- checkExp e
  kenv <- getKenv
  if (equivalent kenv t u) then
    return ()
  else
    addError p ["Expecting type", styleRed (show u),
                 "to be equivalent to type", styleRed (show t)]

-- | Typing rules for expressions

checkExp :: Expression -> TypingState TypeScheme
-- Basic expressions
checkExp (Unit p)         = return $ TypeScheme p [] (Basic p UnitType)
checkExp (Integer p _)    = return $ TypeScheme p [] (Basic p IntType)
checkExp (Character p _)  = return $ TypeScheme p [] (Basic p CharType)
checkExp (Boolean p _)    = return $ TypeScheme p [] (Basic p BoolType)

-- Variables
checkExp (Variable p x)   = checkVar p x

checkExp (UnLet _ (px,x) e1 e2) = do
  t1 <- checkExp e1
  addToVenv px x t1
  t2 <- checkExp e2
  venv <- getVenv
  checkUnVar venv px x
  removeFromVenv x
  return t2
  
-- Applications
checkExp (App p e1 e2) = do
  t <- checkExp e1
  (u1, u2) <- extractFun p t
  checkAgainst p e2 u1
  return u2

checkExp (TypeApp p x ts) = do
  t' <- checkVar p x
  (bs, t) <- extractScheme t'
-- wellFormedCall p e ts binds

  let typeBind = zip ts bs
  mapM (\(t, Bind _ _ k) -> K.checkAgainst k t) typeBind
  -- well formed sub??
  return $ TypeScheme p [] (subL t typeBind)
 
--   t1 <- checkVar p x  
--   (binds, v) <- extractScheme p t1
--   wellFormedCall p e ts binds

--   let typeBind = zip ts binds
--   mapM (\(t,b) -> K.checkAgainst (kind b) t) typeBind
--   let sub = subL v typeBind
--   -- TODO: TEMPORARY
--   kenv <- getKenv
--   if K.isWellFormed sub kenv then 
--     return $ TypeScheme [] sub -- (subL v typeBind) --(subs v "a" (Skip p))
--   else
--     return $ TypeScheme [] (Skip p)
-- --   return $ TypeScheme [] (subL v typeBind)
    
-- Conditional
checkExp (Conditional p e1 e2 e3) = do
  checkAgainst p e1 (TypeScheme p [] (Basic p BoolType))
  venv2 <- getVenv
  t2 <- checkExp e2  
  venv3 <- getVenv
  setVenv venv2
  checkAgainst p e3 t2
  venv4 <- getVenv
  kenv <- getKenv
  checkEquivEnvs kenv venv3 venv4 -- TODO: remove kenv
  setVenv venv2
  return t2

-- Pairs
checkExp (Pair p e1 e2) = do
 (TypeScheme _ bs1 t1) <- checkExp e1 
 (TypeScheme _ bs2 t2) <- checkExp e2 
 return $ TypeScheme p (bs1++bs2) (PairType p t1 t2)

checkExp (BinLet p (px,x) (py,y) e1 e2) = do
  t1 <- checkExp e1
  (u1,u2) <- extractPair p t1                     
  addToVenv px x u1
  addToVenv py y u2
  u <- checkExp e2
  venv <- getVenv
  checkUnVar venv px x
  checkUnVar venv py y
  removeFromVenv x
  removeFromVenv y
  return u

-- Session types

checkExp (New p t) = do
  K.checkAgainst (Kind p Session Lin) t
  -- m <- extractChoiceMap t
  -- Map.foldrWithKey (\c t _ -> addToVenv p c (TypeScheme [] t)) (return ()) m
  return $ TypeScheme p [] (PairType p t (dual t))

checkExp (Send p e1 e2) = do
  t1 <- checkExp e1
  b1 <- extractBasic (position e1) t1 -- TODO: remove pos
  t2 <- checkExp e2
  (b2, u) <- extractOutput p t2
  checkEquivBasics p b1 b2
  return u

checkExp (Receive p e) = do
  venv <- getVenv
  t <- checkExp e
  (b, TypeScheme _ _ t1) <- extractInput (position e) t -- TODO : return a type
  return $ TypeScheme p [] (PairType p (Basic p b) t1)

-- Branching
checkExp (Select p c e) = do --TODO: check with rule
  t <- checkExp e
  (TypeScheme p bs choice) <- extractInChoice p t
  u <- extractConstructor p c choice  
  return $ TypeScheme p bs u

checkExp (Match p e cm) = do
  t1 <- checkExp e
  let (c, (x, e1)) = Map.elemAt 0 cm
  t2 <- extractExtChoice p t1 c
  addToVenv (paramPos x) (param x) t2
  venv <- getVenv
  u <- checkExp e1
  Map.foldrWithKey (\k (v1,v2) _ -> checkMap venv p t1 u k ([v1], v2) extractExtChoice) (return ()) (Map.delete c cm)
  return u

checkExp (Constructor p c) = checkVar p c

checkExp (Fork p e) = do
  t <- checkExp e
  checkUn p t
  return $ TypeScheme p [] (Basic p UnitType)
--  return t
  
checkExp (Case cp e cm) = do
  t <- checkExp e
  let (c, (x, e1)) = Map.elemAt 0 cm  
  t2 <- extractDatatype cp t c  
  let x' = zip x (init (toList t2))        
  foldM (\_ (p, t) -> addToVenv (paramPos p) (param p) t) () x' 
  venv <- getVenv
  u <- checkExp e1
  -- setVEnv venv
  Map.foldrWithKey (\k v _ -> checkMap venv cp t u k v extractDatatype)
                   (return ()) (Map.delete c cm)
  return u

-- | Checking Variables

{- | Checks a variable and removes it from the environment if
     it is linear.
-}
checkVar :: Pos -> TermVar -> TypingState TypeScheme
checkVar p x = do
  member <- venvMember x  
  if member then do
    (_,t@(TypeScheme _ bs _)) <- getFromVenv x
    addBindsToKenv p bs
    kenv <- getKenv
    removeLinVar kenv x t
    return t
  else do
    addError p ["Variable or data constructor not in scope:", styleRed x]
    addToVenv p x (TypeScheme p [] (Basic p UnitType))
    return $ TypeScheme p [] (Basic p UnitType)

addBindsToKenv :: Pos -> [Bind] -> TypingState ()
addBindsToKenv p bs = foldM (\_ (Bind _ b k) -> addToKenv p b k) () bs

removeLinVar :: KindEnv -> TermVar -> TypeScheme -> TypingState ()
removeLinVar kenv x (TypeScheme _ _ t) = do
  isLin <- K.lin t
  if isLin then
    removeFromVenv x
  else
    return ()
 
checkUnVar :: VarEnv -> Pos -> TermVar -> TypingState ()
checkUnVar venv p x
  | Map.member x venv = let (_,x1) = venv Map.! x in checkUn p x1
  | otherwise         = return ()

-- | The Extract Functions

extractFun :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
extractFun _ (TypeScheme p [] (Fun _ _ t u)) = return (TypeScheme p [] t, TypeScheme p [] u)
extractFun p (TypeScheme _ [] t)           = do
  addError p ["Expecting a function type; found:", styleRed $ show t]
  return (TypeScheme p [] (Basic p UnitType), TypeScheme p [] (Basic p UnitType))
extractFun p (TypeScheme _ bs _)           = do
  addError p ["Polymorphic functions cannot be applied; instantiate function prior to applying"]
  return (TypeScheme p [] (Basic p UnitType), TypeScheme p [] (Basic p UnitType))

extractScheme :: TypeScheme -> TypingState ([Bind], Type)
extractScheme (TypeScheme p [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme p bs t) = return (bs, t)

extractPair :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
extractPair p (TypeScheme _ bs (PairType _ t u)) = do
  addBindsToKenv p bs
  return (TypeScheme p bs t, TypeScheme p bs u)
extractPair p t                         = do
  addError p ["Expecting a pair type; found ", styleRed $ show t]
  return (TypeScheme p [] (Basic p IntType), TypeScheme p [] (Basic p IntType))

extractBasic :: Pos -> TypeScheme -> TypingState BasicType
extractBasic _ (TypeScheme p bs (Basic _ t)) = do
  addBindsToKenv p bs
  return t
extractBasic p t                         = do
  addError p ["Expecting a basic type; found", styleRed $ show t]
  return UnitType

  -- !~>
-- TODO: review this case (bindings)
extractOutput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractOutput p (TypeScheme _ bs (Semi _ (Skip _) t)) = do
  addBindsToKenv p bs
  extractOutput p (TypeScheme p bs t)
extractOutput _ (TypeScheme p bs (Semi _ (Message _ Out b) t)) =
  return (b, TypeScheme p bs t)
extractOutput _ (TypeScheme p bs (Message _ Out b)) = return (b, TypeScheme p bs (Skip p)) --TODO: POS
extractOutput p (TypeScheme _ bs (Rec p1 b t)) = do
  extractOutput p (TypeScheme p bs (unfold (Rec p1 b t)))
extractOutput p (TypeScheme _ bs (Semi _ t u)) = do -- TODO: Wrong?
  (b, TypeScheme _ bs' t1) <- extractOutput p (TypeScheme p bs t)
  return (b, TypeScheme p bs' (Semi p t1 u)) -- TODO: Pos
extractOutput p t = do
  addError p ["Expecting an output type; found", styleRed $ show t]
  return (UnitType, TypeScheme p [] (Skip p)) -- TODO: POS

-- TODO: review this case (bindings)
extractInput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractInput p (TypeScheme _ bs (Semi _ (Skip _) t)) = do
  addBindsToKenv p bs
  extractInput p (TypeScheme p bs t)
extractInput _ (TypeScheme p bs (Semi _ (Message _ In b) t)) =
  return (b, TypeScheme p bs t)
extractInput _ (TypeScheme _ bs (Message p In b)) = return (b, TypeScheme p [] (Skip p)) --TODO: Pos
extractInput p (TypeScheme _ bs (Rec pr b t)) = do
  addBindsToKenv p bs
  extractInput p (TypeScheme p bs (unfold (Rec pr b t)))
extractInput p (TypeScheme _ bs (Semi _ t u)) = do -- TODO: Wrong?
  (b, TypeScheme p _ t1) <- extractInput p (TypeScheme p bs t)
  return (b, TypeScheme p bs (Semi p t1 u)) --TODO: Pos
extractInput p t = do
  addError p ["Expecting an input type; found", styleRed $ show t]
  return (UnitType, TypeScheme p [] (Skip p)) --TODO: Pos


-- TODO: review this case (bindings)
extractInChoice :: Pos -> TypeScheme -> TypingState TypeScheme
extractInChoice p (TypeScheme _ bs (Semi _ (Skip _) t)) = do
  addBindsToKenv p bs
  extractInChoice p (TypeScheme p bs t)
extractInChoice _ (TypeScheme _ bs (Choice p Internal m)) = return $ TypeScheme p bs (Choice p Internal m)
extractInChoice _ (TypeScheme _ bs (Semi p (Choice p1 Internal m) t)) =
  return $ TypeScheme p bs (Choice p1 Internal (Map.map (\t1 -> Semi p t1 t) m)) -- TODO: Pos
--  return $ TypeScheme bs (Choice Internal (Map.map (`Semi` t) m))
extractInChoice p (TypeScheme _ bs (Rec pr b t)) = do
  addBindsToKenv p bs
  extractInChoice p (TypeScheme p bs (unfold (Rec pr b t)))
extractInChoice p (TypeScheme _ bs (Semi _ (Semi _ t1 t2) t3)) = do
  (TypeScheme _ _ t4) <- extractInChoice p (TypeScheme p bs (Semi p t1 t2)) --TODO: Pos
  extractInChoice p (TypeScheme p bs (Semi p t4 t3))  -- TODO: Pos
extractInChoice p (TypeScheme _ bs (Semi _ t1 t2)) = do
  (TypeScheme _ bs' t3) <- extractInChoice p (TypeScheme p bs t1)
  extractInChoice p (TypeScheme p bs (Semi p t3 t2))
extractInChoice p t = do
  addError p ["Expecting an internal choice; found", styleRed $ show t]
  return $ TypeScheme p [] (Skip p) --TODO: Pos

extractConstructor :: Pos -> Constructor -> Type -> TypingState Type
extractConstructor p c (Choice _ _ tm) =
  if Map.member c tm then
    return $ tm Map.! c 
  else do
    addError p ["Constructor", styleRed $ "'"++c++"'", "not in scope"]             
    return (Basic p UnitType)
extractConstructor p c t = do
  addError p ["Expecting a choice; found", styleRed $ show t]
  return (Basic p UnitType)

-- TODO: review this case (bindings)
-- TODO: error on Map.!
extractExtChoice :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractExtChoice _ (TypeScheme p bs (Semi _ (Skip _) t)) c =
  extractExtChoice p (TypeScheme p bs t) c
extractExtChoice _ t@(TypeScheme p bs (Choice _ External m)) c =
  if not (Map.member c m) then do
    addError p ["Choice label not in scope", styleRed $ show t, "\n", styleRed $ show c]    
    return $ TypeScheme p [] (Skip p) --TODO: Pos
  else
   return $ TypeScheme p bs (m Map.! c) -- Choice External m

extractExtChoice _ (TypeScheme p bs (Semi _ (Choice _ External m) t)) c =
  return $ TypeScheme p bs ((Map.map (\t1 -> Semi p t1 t) m) Map.! c) -- TODO:Pos
extractExtChoice _ (TypeScheme p bs r@(Rec _ _ _)) c = do
  addBindsToKenv p bs  
  extractExtChoice p (TypeScheme p bs (unfold r)) c
--  return b1
extractExtChoice _ (TypeScheme p bs (Semi _ t1 t2)) c = do  
  addBindsToKenv p bs  
  (TypeScheme _ _ t3) <- extractExtChoice p (TypeScheme p bs t1) c
  return $ TypeScheme p bs (Semi p t3 t2)  -- TODO: Pos
extractExtChoice p t _ = do
  addError p ["Expecting an external choice; found", styleRed $ show t]    
  return $ TypeScheme p [] (Skip p) --TODO: Pos

-- TODO: review this case (bindings)
extractDatatype :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractDatatype _ (TypeScheme p bs (Datatype _ m)) c = do
  addBindsToKenv p bs
  return $ TypeScheme p bs (m Map.! c)
extractDatatype _ (TypeScheme p bs r@(Rec _ _ _)) c =
  extractDatatype p (TypeScheme p bs (unfold r)) c
extractDatatype _ (TypeScheme p [] (Var pv x)) c = do -- Should be here?
  b <- venvMember x
  if b then do
    (_,dt) <- getFromVenv x
    extractDatatype p dt c
  else do
    addError p ["Expecting a datatype; found", styleRed $ show (Var pv x)]
    return $ TypeScheme p [] (Basic p IntType)  
-- TODO ??
-- extractDatatype p (TypeScheme [] (Semi t1 t2)) = do
extractDatatype p t _ = do
  addError p ["Expecting a datatype; found", styleRed $ show t]
  return $ TypeScheme p [] (Basic p IntType)  


-- | AUXILIARY FUNCTIONS

{- | Verifies if x is well formed (e[x] based on the kind)
   | Checks if all x1,...,xn in e[x1,...,xn] are well kinded
   | Checks if the number of types (n) are admited by type
-}
  
  -- TODO: TEST
wellFormedCall :: Pos -> Expression -> [Type] -> [Bind] -> TypingState ()
wellFormedCall p e ts binds = do
  mapM_ (\t -> K.kinding (TypeScheme p [] t)) ts
  sameNumber
  where   
    sameNumber
      | length binds == length ts = return ()
      | otherwise                 =          
          addError p ["Expecting", show $ length binds,
                      "type(s) on type app; found", show $ length ts]


-- | TODO: myinit ? 
checkMap :: VarEnv -> Pos -> TypeScheme -> TypeScheme -> TermVar ->
            (Params, Expression) ->
            (Pos -> TypeScheme -> Constructor -> TypingState TypeScheme) ->
            TypingState ()
            
checkMap venv mp choice against c (x, e) f = do 
  setVenv venv
  t1 <- f mp choice c  
  let x' = zip x (myInit (toList t1))
  foldM (\_ (p, t) -> addToVenv (paramPos p) (param p) t) () x' 
  checkAgainst mp e against
  return ()

myInit :: [a] -> [a]
myInit (x:[]) = [x]
myInit x = init x


-- | Equivalence functions


-- | TODO: position, diff, better error message, maybe with diff between maps
-- and something else (only compares keys)
checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState ()
checkEquivEnvs kenv venv1 venv2 = do
  equiv <- equivalentEnvs kenv venv1 venv2
  if equiv then
    return ()
  else
    let (_,(tmp,_)) = Map.elemAt 0 venv1 in 
      addError tmp ["Expecting environment", show venv1,
                    "to be equivalent to environment", show venv2]      
 -- | TODO: position, diff, better error message, maybe with diff between maps
equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState Bool
equivalentEnvs kenv venv1 venv2 = do
  venv3 <- Map.foldlWithKey f1 (return Map.empty) venv1
  venv4 <- Map.foldlWithKey f1 (return Map.empty) venv2
  return $ Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
  return True
  where
    f (_,(TypeScheme _ _ t)) (_,(TypeScheme _ _ u)) = equivalent kenv t u
    f1 :: TypingState VarEnv -> TermVar -> (Pos,TypeScheme) -> TypingState VarEnv
    f1 m k t@(_,(TypeScheme _ _ t1)) = do
      isLin <- K.lin t1

      if isLin then do
        m1 <- m        
        return $ Map.insert k t m1
      else m


-- WAS
-- -- and something else (only compares keys)
-- checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState ()
-- checkEquivEnvs kenv venv1 venv2 -- = return ()
--   | equivalentEnvs kenv venv1 venv2  = return ()
--   | otherwise =
--       let (_,(tmp,_)) = Map.elemAt 0 venv1 in 
--       addError tmp ["Expecting environment", show venv1,
--                     "to be equivalent to environment", show venv2]
--
-- equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
-- equivalentEnvs kenv venv1 venv2 =
--   let venv3 = Map.filter f1 venv1
--       venv4 = Map.filter f1 venv2 in
--   Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
--   where
--     f (_,(TypeScheme _ t)) (_,(TypeScheme _ u)) = equivalent kenv t u
--     f1 (_,(TypeScheme _ t)) = K.lin kenv t

checkEquivBasics :: Pos -> BasicType -> BasicType -> TypingState ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise =
      addError p ["Expecting basic type", styleRed $ show b1,
                  "to be equivalent to basic type", styleRed $ show b2]

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)
