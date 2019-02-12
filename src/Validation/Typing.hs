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
import           Data.List ((\\), nub, intercalate)
import qualified Data.Set as Set
import qualified Data.Traversable as Trav
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Validation.Kinding
import           Validation.TypeEquivalence
import           Validation.TypingState
import           System.Console.Pretty (Color (..), Style (..), color, style)
import Utils.Errors
-- TODO: tmp
import Parse.Parser
import PreludeLoader

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey (f) m)

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
--   venv <- getVenv
  --
  mapWithKeyM (\fun (_, a, e) -> checkFD venv2 fun a e) eenv

  venv <- getVenv
  Trav.mapM (\(p, t) -> checkUn p t) venv
  return ()


-- | temporary function to remove prelude from venv
--   it makes debugging easier
removePrelude venv = Map.foldlWithKey (\acc x _ -> Map.delete x acc) venv prelude

-- | AUXILIARY FUNCTIONS TO VERIFY DATATYPES

{- | Checks all the datatypes definitions:
   |  - Checks if they are well kinded and if they have a functional kind
-}
checkDataDecl :: TypingState ()
checkDataDecl = do 
  kenv <- getKenv
  mapM_ (\k -> checkFunctionalKind k) kenv
  cenv <- getCenv
  mapM_ (\(p,t) -> checkKinding p t) cenv


checkFunctionalKind :: (Pos, Kind) -> TypingState ()
checkFunctionalKind (p, k)
  | k >= (Kind Functional Un) = return ()
  | otherwise = do
     file <- getFileName
     addError $ styleError file p
                [ "Expecting a functional (TU or TL) type; found a",
                   styleRed (show k), "type."]

       -- (prettyErrorHeader file p) ++
       -- (styleMsg $ "Expecting a functional (TU or TL) type; found a " ++ (show k) ++ " type.") 

checkKinding :: Pos -> TypeScheme -> TypingState ()
checkKinding p t = do
  kinding p t
  return ()

-- | AUXILIARY FUNCTIONS TO VERIFY FUNCTION TYPES

-- | Verifies if a function exists and if it is well kinded
checkFunTypeDecl :: Pos -> TermVar -> TypingState ()
checkFunTypeDecl pos fname = do  
  (p,t) <- checkFun pos fname
  kinding p t
  return ()

checkFun :: Pos -> TermVar -> TypingState (Pos, TypeScheme)
checkFun pos x = do
  member <- venvMember x
  if member then do
    (p, (TypeScheme bs t)) <- getFromVenv x
    return (p,(TypeScheme bs t))
  else do
    file <- getFileName
    addError $ styleError file pos
               ["Function", styleRed ("'" ++ x ++ "'"), "not in scope"]
    addToVenv pos x (TypeScheme [] (Basic UnitType))
    return $ (pos, TypeScheme [] (Basic UnitType))


-- | FUNCTION DECLARATION

{- | Checks a function declaration:
   |  - Checks the function form
   |  - Checks the function body (expression) against the declared type
   |  - Checks if the resulting environment is unrestricted
-}
checkFD ::  VarEnv -> TermVar -> Params -> Expression -> TypingState ()
checkFD venv fname p exp = do

  checkFunForm venv fname p
--  venv <- getVenv
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
-- TODO: refector params to args
checkFunForm :: VarEnv -> TermVar -> Params -> TypingState ()
checkFunForm venv fun params = do
  checkArgsConflits fun params
--  (p,t) <- getFromVenv fun
  let (p,t) = venv Map.! fun
--  parameters <- checkArgs p fun params (init (toList t))
  parameters <- checkArgs p fun params (normalizeType (init (toList t)))
  foldM (\acc (arg, t) -> addToVenv p arg t) () parameters
  return ()

checkArgsConflits :: TermVar -> Params -> TypingState ()
checkArgsConflits fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     file <- getFileName
     (p,_,_) <- getFromEenv fun
     mapM (err file p) clash
     return ()
  where
    err file p c = addError $ styleError file p
                     ["Conflicting definitions for", showClashes c,
                      "\n\t In an equation for", styleRed $ "'" ++ fun ++ "'"]
    clash = args \\ nub args
    showClashes x = styleRed $ "'" ++ x ++ "'"

checkArgs :: Pos -> TypeVar -> Params -> [TypeScheme] -> TypingState [(TypeVar, TypeScheme)]
checkArgs p c ps ts
  | length ps == length ts = return $ zip ps ts
  | length ps > length ts = do
      file <- getFileName
      addError $ styleError file p
                 ["Function or constructor", styleRed $ "'" ++ c ++ "'",
                  "is applied to too many arguments"]
      return []
  | length ps < length ts = do
      file <- getFileName
      addError $ styleError file p
                 ["Function or constructor", styleRed $ "'" ++ c ++ "'",
                  "is applied to too few arguments"]
      return []


-- | TODO: check this ... Temporary... TESTING

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
     tcvar b (PairType t1 t2) = tcvar b t1 ++ tcvar b t2
     -- DataType
     tcvar b t = error $ "INTERNAL ERROR: " ++ show b ++ " " ++ show t



-- | Checks if a type is unrestricted
checkUn :: Pos -> TypeScheme -> TypingState ()
checkUn p (TypeScheme _ t) = do
  kenv <- getKenv
  if un kenv t then    
    return ()
  else do
    file <- getFileName
    addError $ styleError file p
               ["Type", styleRed $ "'" ++ show t ++ "'",
                "is not unrestricted"]

-- | Checks an expression against a given type
checkAgainst :: Pos -> Expression -> TypeScheme -> TypingState ()
checkAgainst p e (TypeScheme bs1 t) = do
  (TypeScheme bs2 u) <- checkExp e
  kenv <- getKenv
  if (equivalent kenv t u) then
    return ()
  else do
    file <- getFileName
    addError $ styleError file p
                ["Expecting type", styleRed (show u),
                 "to be equivalent to type", styleRed (show t)]
    -- addError $ (prettyErrorHeader file p) ++
    --       (styleMsg $ "Expecting type " ++ (color Red (show u)) ++
    --       (styleMsg $" to be equivalent to type " ++ (color Red (show t))))


-- | Typing rules for expressions

checkExp :: Expression -> TypingState TypeScheme
-- Basic expressions
checkExp (Unit _)         = return $ TypeScheme [] (Basic UnitType)
checkExp (Integer _ _)    = return $ TypeScheme [] (Basic IntType)
checkExp (Character _ _)  = return $ TypeScheme [] (Basic CharType)
checkExp (Boolean _ _)    = return $ TypeScheme [] (Basic BoolType)

-- Variables
checkExp (Variable p x)   = checkVar p x

checkExp (UnLet p x e1 e2) = do
  t1 <- checkExp e1
  addToVenv p x t1
  t2 <- checkExp e2

  -- TODO: NEW TEST
  -- really need?, if yes implement in other function to provide a suitable error message
  -- if x is used then it is removed, must check
  -- (_,t3) <- getFromVenv x
  -- checkUn p t3
  -- END
   
  removeFromVenv x
  return t2
  
-- Applications
checkExp (App p e1 e2) = do
  t <- checkExp e1
  (u1, u2) <- extractFun p t
  checkAgainst p e2 u1
  return u2

checkExp (TypeApp p e ts) = do
  t1 <- checkExp e  
  (binds, t2) <- extractScheme p t1
  wellFormedCall p e ts binds
  
  -- TODO: move to other module and call subL
  let sub = foldr (\(t', b) acc -> subs t' (var b) acc) t2 (zip ts binds)
  kenv <- getKenv
  
  -- TODO: the result type is well formed  
  return $ TypeScheme [] sub

-- Conditional
checkExp (Conditional p e1 e2 e3) = do
  checkAgainst p e1 (TypeScheme [] (Basic BoolType))
  venv2 <- getVenv
  t2 <- checkExp e2  
  venv3 <- getVenv
  setVenv venv2
  checkAgainst p e3 t2
  venv4 <- getVenv
  kenv <- getKenv
  checkEquivEnvs kenv venv3 venv4
  setVenv venv2
  return t2

-- Pairs
checkExp (Pair p e1 e2) = do
 (TypeScheme bs1 t1) <- checkExp e1 
 (TypeScheme bs2 t2) <- checkExp e2 
 return $ TypeScheme (bs1++bs2) (PairType t1 t2)

checkExp (BinLet p x y e1 e2) = do
  t1 <- checkExp e1
  (u1,u2) <- extractPair p t1                     
  addToVenv p x u1
  addToVenv p y u2
  u <- checkExp e2
  -- | TODO TESTAR TUDO
  venv <- getVenv
  checkUnVar venv p x 
  checkUnVar venv p y

  removeFromVenv x
  removeFromVenv y
  return u

-- Session types

checkExp (New p t) = do
  checkAgainstKind p t (Kind Session Lin)
  -- m <- extractChoiceMap t
  -- Map.foldrWithKey (\c t _ -> addToVenv p c (TypeScheme [] t)) (return ()) m
  return $ TypeScheme [] (PairType t (dual t))

checkExp (Send p e1 e2) = do
  venv <- getVenv
  t1 <- checkExp e1
  b1 <- extractBasic (getEPos e1) t1
  t2 <- checkExp e2
  (b2, u) <- extractOutput p t2
  checkEquivBasics p b1 b2
  return u

checkExp (Receive _ e) = do
  venv <- getVenv
  t <- checkExp e
  (b, TypeScheme bs t1) <- extractInput (getEPos e) t
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
  addToVenv p x t2
  venv <- getVenv
  u <- checkExp e1
  Map.foldrWithKey (\k (v1,v2) _ -> checkMap venv p t1 u k ([v1], v2) extractExtChoice) (return ()) (Map.delete c cm)
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
  foldM (\_ (p, t) -> addToVenv pos p t) () x' 
  venv <- getVenv
  
  u <- checkExp e1
  -- setVEnv venv
  Map.foldrWithKey (\k v _ -> checkMap venv pos t u k v extractDatatype)
                   (return ()) (Map.delete c cm)
  return u
  


-- Extract Without Errors
-- TODO: check: need?
-- extractChoiceMap :: Type -> TypingState TypeMap
-- extractChoiceMap (Choice _ m) = return m
-- extractChoiceMap (Semi (Choice _ m) t2) = return (Map.map (`Semi` t2) m)
-- extractChoiceMap (Semi t1 t2) = do
--   m1 <- extractChoiceMap t1
--   m2 <- extractChoiceMap t2
--   return $ Map.union m1 m2
-- extractChoiceMap  _ = return Map.empty


-- | Checking Variables

{- | Checks a variable and removes it from the environment if
     it is linear.
-}
checkVar :: Pos -> TermVar -> TypingState TypeScheme
checkVar p x = do
  member <- venvMember x  
  if member then do
    (_,t@(TypeScheme bs _)) <- getFromVenv x
    addBindsToKenv p bs
    kenv <- getKenv
    removeLinVar kenv x t
    return t
  else do
    file <- getFileName
    addError $ styleError file p
               ["Variable or data constructor not in scope:", styleRed x]
    addToVenv p x (TypeScheme [] (Basic UnitType))
    return $ TypeScheme [] (Basic UnitType)

addBindsToKenv :: Pos -> [Bind] -> TypingState ()
addBindsToKenv p bs = foldM (\_ b -> addToKenv p (var b) (kind b)) () bs

removeLinVar :: KindEnv -> TermVar -> TypeScheme -> TypingState ()
removeLinVar kenv x (TypeScheme _ t)
  | lin kenv t = removeFromVenv x   
  | otherwise  = return ()

checkUnVar :: VarEnv -> Pos -> TermVar -> TypingState ()
checkUnVar venv p x
  | Map.member x venv = let (_,x1) = venv Map.! x in checkUn p x1
  | otherwise         = return ()

-- | The Extract Functions

extractFun :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
extractFun _ (TypeScheme [] (Fun _ t u)) = return (TypeScheme [] t, TypeScheme [] u)
extractFun p (TypeScheme [] t)           = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting a function type; found:", styleRed $ show t]
  return (TypeScheme [] (Basic UnitType), TypeScheme [] (Basic UnitType))
extractFun p (TypeScheme bs _)           = do
  file <- getFileName
  addError $ styleError file p
             ["Polymorphic functions cannot be applied; instantiate function prior to applying"]
  return (TypeScheme [] (Basic UnitType), TypeScheme [] (Basic UnitType))


extractScheme :: Pos -> TypeScheme -> TypingState ([Bind], Type)
extractScheme p (TypeScheme [] t) = do
  file <- getFileName 
  addError $ styleError file p
             ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic UnitType))
extractScheme _ (TypeScheme bs t) = return (bs, t)

-- | TODO TESTAR
extractPair :: Pos -> TypeScheme -> TypingState (TypeScheme, TypeScheme)
extractPair p (TypeScheme bs (PairType t u)) = do
  addBindsToKenv p bs
  return (TypeScheme bs t, TypeScheme bs u)
extractPair p t                         = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting a pair type; found ", styleRed $ show t]
  return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))

-- | TODO TESTAR
extractBasic :: Pos -> TypeScheme -> TypingState BasicType
extractBasic p (TypeScheme bs (Basic t)) = do
  addBindsToKenv p bs
  return t
extractBasic p t                         = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting a basic type; found", styleRed $ show t]
  return UnitType

  -- !~>
-- TODO: review this case (bindings)
extractOutput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractOutput p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv p bs
  extractOutput p (TypeScheme bs t)
extractOutput _ (TypeScheme bs (Semi (Message Out b) t)) = return (b, TypeScheme bs t)
extractOutput _ (TypeScheme bs (Message Out b)) = return (b, TypeScheme bs Skip)
extractOutput p (TypeScheme bs (Rec b t)) = do
  extractOutput p (TypeScheme bs (unfold (Rec b t)))
extractOutput p (TypeScheme bs (Semi t u)) = do -- TODO: Wrong?
  (b, TypeScheme bs' t1) <- extractOutput p (TypeScheme bs t)
  return (b, TypeScheme bs' (t1 `Semi` u))
extractOutput p t = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting an output type; found", styleRed $ show t]
  return (UnitType, TypeScheme [] Skip)

-- TODO: review this case (bindings)
extractInput :: Pos -> TypeScheme -> TypingState (BasicType, TypeScheme)
extractInput p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv p bs
  extractInput p (TypeScheme bs t)
extractInput _ (TypeScheme bs (Semi (Message In b) t)) =
  return (b, TypeScheme bs t)
extractInput _ (TypeScheme bs (Message In b)) = return (b, TypeScheme [] Skip)
extractInput p (TypeScheme bs (Rec b t)) = do
  addBindsToKenv p bs
  extractInput p (TypeScheme bs (unfold (Rec b t)))
extractInput p (TypeScheme bs (Semi t u)) = do -- TODO: Wrong?
  (b, TypeScheme _ t1) <- extractInput p (TypeScheme bs t)
  return (b, TypeScheme bs (t1 `Semi` u))
extractInput p t = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting an input type; found", styleRed $ show t]
  return (UnitType, TypeScheme [] Skip)


-- TODO: review this case (bindings)
extractInChoice :: Pos -> TypeScheme -> TypingState TypeScheme
extractInChoice p (TypeScheme bs (Semi Skip t)) = do
  addBindsToKenv p bs
  extractInChoice p (TypeScheme bs t)
extractInChoice _ (TypeScheme bs (Choice Internal m)) = return $ TypeScheme bs (Choice Internal m)
extractInChoice _ (TypeScheme bs (Semi (Choice Internal m) t)) =
  return $ TypeScheme bs (Choice Internal (Map.map (`Semi` t) m))
extractInChoice p (TypeScheme bs (Rec b t)) = do
  addBindsToKenv p bs
  extractInChoice p (TypeScheme bs (unfold (Rec b t)))
extractInChoice p (TypeScheme bs (Semi (Semi t1 t2) t3)) = do
  (TypeScheme _ t4) <- extractInChoice p (TypeScheme bs (Semi t1 t2))
  extractInChoice p (TypeScheme bs (Semi t4 t3))  
extractInChoice p (TypeScheme bs (Semi t1 t2)) = do
  (TypeScheme bs' t3) <- extractInChoice p (TypeScheme bs t1)
  extractInChoice p (TypeScheme bs (Semi t3 t2))
extractInChoice p t = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting an internal choice; found", styleRed $ show t]
  return $ TypeScheme [] Skip 

extractConstructor :: Pos -> Constructor -> Type -> TypingState Type
extractConstructor p c (Choice _ tm) =
  if Map.member c tm then
    return $ tm Map.! c 
  else do
    file <- getFileName
    addError $ styleError file p
             ["Constructor", styleRed $ "'"++c++"'", "not in scope"]             
    return (Basic UnitType)
extractConstructor p c t = do
  file <- getFileName
  addError $ styleError file p
             ["Expecting a choice; found", styleRed $ show t]
  return (Basic UnitType)

-- TODO: review this case (bindings)
extractExtChoice :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractExtChoice p (TypeScheme bs (Semi Skip t)) c = extractExtChoice p (TypeScheme bs t) c
extractExtChoice p t@(TypeScheme bs (Choice External m)) c =
  if not (Map.member c m) then do
    file <- getFileName
    addError $ styleError file p
               ["Choice label not in scope", styleRed $ show t, "\n", styleRed $ show c]    
    return $ TypeScheme [] Skip
  else
   return $ TypeScheme bs (m Map.! c) -- Choice External m

extractExtChoice _ (TypeScheme bs (Semi (Choice External m) t)) c =
  return $ TypeScheme bs ((Map.map (`Semi` t) m) Map.! c)
extractExtChoice p (TypeScheme bs r@(Rec b t)) c = do
  addBindsToKenv p bs  
  extractExtChoice p (TypeScheme bs (unfold r)) c
--  return b1
extractExtChoice p (TypeScheme bs (Semi t1 t2)) c = do  
  addBindsToKenv p bs  
  (TypeScheme _ t3) <- extractExtChoice p (TypeScheme bs t1) c
  return $ TypeScheme bs (Semi t3 t2)  
extractExtChoice p t _ = do
  file <- getFileName
  addError $ styleError file p
            ["Expecting an external choice; found", styleRed $ show t]    
  return $ TypeScheme [] Skip

-- TODO: review this case (bindings)
extractDatatype :: Pos -> TypeScheme -> Constructor -> TypingState TypeScheme
extractDatatype p (TypeScheme bs (Datatype m)) c = do
  addBindsToKenv p bs
  return $ TypeScheme bs (m Map.! c)
extractDatatype p (TypeScheme bs r@(Rec b t)) c =
  extractDatatype p (TypeScheme bs (unfold r)) c
extractDatatype p (TypeScheme _ (Var x)) c = do -- Should be here?
  b <- venvMember x
  if b then do
    (_,dt) <- getFromVenv x
    extractDatatype p dt c
  else do
    file <- getFileName
    addError $ styleError file p
               ["Expecting a datatype; found", styleRed $ show (Var x)]
    return $ TypeScheme [] (Basic IntType)  
-- TODO ??
-- extractDatatype p (TypeScheme [] (Semi t1 t2)) = do
extractDatatype p t _ = do
  file <- getFileName
  addError $ styleError file p
               ["Expecting a datatype; found", styleRed $ show t]
  return $ TypeScheme [] (Basic IntType)  


-- | AUXILIARY FUNCTIONS

{- | Verifies if x is well formed (e[x] based on the kind)
   | Checks if all x1,...,xn in e[x1,...,xn] are well kinded
   | Checks if the number of types (n) are admited by type
-}
  
  -- TODO: TEST
wellFormedCall :: Pos -> Expression -> [Type] -> [Bind] -> TypingState ()
wellFormedCall p e ts binds = do
  mapM_ (\t -> kinding p (TypeScheme [] t)) ts
  sameNumber
  where   
    sameNumber
      | length binds == length ts = return ()
      | otherwise                 = do
          file <- getFileName
          addError $ styleError file p
                     ["Expecting", show $ length binds,
                      "type(s) on type app; found", show $ length ts]


-- | TODO: myinit ? 
checkMap :: VarEnv -> Pos -> TypeScheme -> TypeScheme -> TermVar ->
            (Params, Expression) ->
            (Pos -> TypeScheme -> Constructor -> TypingState TypeScheme) ->
            TypingState ()
            
checkMap venv pos choice against c (x, e) f = do 
  setVenv venv
  t1 <- f pos choice c  
  let x' = zip x (myInit (toList t1))
  foldM (\_ (p, t) -> addToVenv pos p t) () x' 
  checkAgainst pos e against
  return ()

myInit :: [a] -> [a]
myInit (x:[]) = [x]
myInit x = init x


-- | Equivalence functions


-- | TODO: position diff, better error message, maybe with diff between maps
-- and something else (only compares keys)
checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState ()
checkEquivEnvs kenv venv1 venv2 -- = return ()
  | equivalentEnvs kenv venv1 venv2  = return ()
  | otherwise = do
      file <- getFileName
      addError $ styleError file (0,0)
                 ["Expecting environment", show venv1,
                  "to be equivalent to environment", show venv2]      
      -- addError ("Expecting environment " ++ show venv1 ++
      --                  " to be equivalent to environment " ++ show venv2)

equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
equivalentEnvs kenv venv1 venv2 =
  let venv3 = Map.filter f1 venv1
      venv4 = Map.filter f1 venv2 in
  Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
  where
    f (_,(TypeScheme _ t)) (_,(TypeScheme _ u)) = equivalent kenv t u
    f1 (_,(TypeScheme _ t)) = lin kenv t


checkEquivBasics :: Pos -> BasicType -> BasicType -> TypingState ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise =  do
      file <- getFileName
      addError $ styleError file p
                 ["Expecting basic type", styleRed $ show b1,
                  "to be equivalent to basic type", styleRed $ show b2]
