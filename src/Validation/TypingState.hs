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

module Validation.TypingState where

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Utils.Errors

-- | The typing state
type Errors = [String]

data TypingS = TypingS {
  filename :: String,
  varEnv :: VarEnv,
  expEnv :: ExpEnv,
  consEnv :: ConstructorEnv,
  kindEnv :: KindEnv,
  errors :: Errors,
  fv :: Int} deriving Show -- TODO: use a record

type TypingState = State TypingS
-- | State manipulating functions

-- | Initial State
-- (_,_,_,_,_,_)
-- (f, venv, eenv, cenv, kenv, err, n)

-- initialState :: String ->
--                  (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors, Int)
-- initialState f = (f, Map.empty, Map.empty, Map.empty, Map.empty, [], 0)

initialState :: String -> TypingS
initialState f = TypingS {filename=f,
                          varEnv=Map.empty,
                          expEnv=Map.empty,
                          consEnv=Map.empty,
                          kindEnv=Map.empty,
                          errors=[],
                          fv=0}


-- | FILE NAME

-- getFileName :: TypingState String
-- getFileName = do
--   (f,_,_,_,_,_,_) <- get
--   return f

getFileName :: TypingState String
getFileName = do
  s <- get
  return $ filename s

-- | VAR ENV

-- getVenv :: TypingState VarEnv
-- getVenv = do
--   (_,venv,_,_,_,_,_) <- get
--   return venv

getVenv :: TypingState VarEnv
getVenv = do
  s <- get
  return $ varEnv s

-- getFromVenv :: Bind -> TypingState TypeScheme
-- getFromVenv x = do
--   venv <- getVenv
--   return $ venv Map.! x

getFromVenv :: Bind -> TypingState TypeScheme
getFromVenv x = do
  venv <- getVenv
  return $ venv Map.! x

-- removeFromVenv :: Bind -> TypingState ()
-- removeFromVenv x =
--   modify (\(f, venv, eenv, cenv, kenv, e, n) ->
--             (f, Map.delete x venv, eenv, cenv, kenv, e, n))

removeFromVenv :: Bind -> TypingState ()
removeFromVenv x =
  modify (\s -> s {varEnv= Map.delete x (varEnv s)})  

  
-- addToVenv :: Bind -> TypeScheme -> TypingState ()
-- addToVenv b t =
--   modify (\(f, venv, eenv, cenv, kenv, e, n) ->
--             (f, Map.insert b t venv, eenv, cenv, kenv, e, n))

addToVenv :: Bind -> TypeScheme -> TypingState ()
addToVenv b t =
  modify (\s -> s{varEnv=Map.insert b t (varEnv s)})

-- venvMember :: Bind -> TypingState Bool
-- venvMember x = do
--   venv <- getVenv
--   return $ Map.member x venv

venvMember :: Bind -> TypingState Bool
venvMember x = do
  venv <- getVenv
  return $ Map.member x venv


-- setVenv :: VarEnv -> TypingState ()
-- setVenv venv = modify (\(f, _, eenv, cenv, kenv, e, n) ->
--                          (f, venv, eenv, cenv, kenv, e, n))

setVenv :: VarEnv -> TypingState ()
setVenv venv = modify (\s -> s{varEnv=venv})

-- -- | EXP ENV

-- getEenv :: TypingState ExpEnv
-- getEenv = do
--   (_,_,eenv,_,_,_,_) <- get
--   return eenv

getEenv :: TypingState ExpEnv
getEenv = do
  s <- get
  return $ expEnv s

-- -- Unsafe - must exist
-- getFromEenv :: Bind -> TypingState ([Bind], Expression)
-- getFromEenv x = do
--   eenv <- getEenv
--   return $ eenv Map.! x

getFromEenv :: Bind -> TypingState ([Bind], Expression)
getFromEenv x = do
  eenv <- getEenv
  return $ eenv Map.! x
     
-- -- | CONSTRUCTOR ENV
-- getCenv :: TypingState ConstructorEnv
-- getCenv = do
--   (_,_,_,cenv,_,_,_) <- get
--   return cenv

getCenv :: TypingState ConstructorEnv
getCenv = do
  s <- get
  return $ consEnv s


-- -- | KIND ENV
-- getKenv :: TypingState KindEnv
-- getKenv = do
--   (_,_,_,_,kenv,_,_) <- get
--   return kenv

getKenv :: TypingState KindEnv
getKenv = do
  s <- get
  return $ kindEnv s

-- addToKenv :: Bind -> Kind -> TypingState ()
-- addToKenv x k =
--   modify (\(f, venv, eenv, cenv, kenv, e, n) ->
--             (f, venv, eenv, cenv, Map.insert x k kenv, e, n))

addToKenv :: Bind -> Kind -> TypingState ()
addToKenv x k =
  modify (\s -> s {kindEnv=Map.insert x k (kindEnv s)})

-- kenvMember :: Bind -> TypingState Bool
-- kenvMember x = do
--   kenv <- getKenv
--   return $ Map.member x kenv

kenvMember :: Bind -> TypingState Bool
kenvMember x = do
  kenv <- getKenv
  return $ Map.member x kenv

-- getKind :: Bind -> TypingState Kind
-- getKind x = do
--   kenv <- getKenv
--   return $ kenv Map.! x 

getKind :: Bind -> TypingState Kind
getKind x = do
  kenv <- getKenv
  return $ kenv Map.! x 


-- removeFromKenv :: Bind -> TypingState ()
-- removeFromKenv x = do
--   kenv <- getKenv
--   if Map.member x kenv then
--     modify (\(f, venv, eenv, cenv, kenv, e, n) ->
--               (f, venv, eenv, cenv, Map.delete x kenv, e, n))
--   else
--     return ()      

removeFromKenv :: Bind -> TypingState ()
removeFromKenv x =
  modify (\s -> s {kindEnv=Map.delete x (kindEnv s)})
  

-- -- ERRORS

-- addError :: Pos -> [String] -> TypingState ()
-- addError p e = do
--   file <- getFileName 
--   modify (\(f, venv, eenv, cenv, kenv, e', n) ->
--             (f, venv, eenv, cenv, kenv,  e' ++ [styleError file p e], n))

addError :: Pos -> [String] -> TypingState ()
addError p e = do
  modify (\s -> s {errors=(errors s) ++ [styleError (filename s) p e]})  
  
-- addErrorList :: [String] -> TypingState ()
-- addErrorList es =
--    modify (\(f, venv, eenv, cenv, kenv, e, n) ->
--             (f, venv, eenv, cenv, kenv, e ++ es, n))

addErrorList :: [String] -> TypingState ()
addErrorList es =
  modify (\s -> s {errors=(errors s) ++ es})   


-- -- FRESH VARS

-- freshVar :: TypingState String
-- freshVar = do
--   (f, venv, eenv, cenv, kenv, e, n) <- get
--   put (f, venv, eenv, cenv, kenv, e, n+1)
--   return $ "_x" ++ show n

freshVar :: TypingState String
freshVar = do
  s <- get
  put $ s {fv=(fv s) +1} -- (f, venv, eenv, cenv, kenv, e, n+1)
  return $ "_x" ++ (show (fv s))
