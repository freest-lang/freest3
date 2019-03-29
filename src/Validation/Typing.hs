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

{-# LANGUAGE LambdaCase #-}
module Validation.Typing
( typeCheck
) where

import           Parse.Lexer (position, defaultPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Exps (Expression (..))
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Validation.Kinding as K
import           Validation.TypingExps
import           Validation.Extract
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Trav

typeCheck :: FreestState ()
typeCheck = do
  -- 2 - Data declaration
  -- Checks if the datatypes are well kinded
  checkDataDecl
  
  -- 3 - Function type declaration
  -- Checks if all function types are well kinded and if
  -- all the declared functions have types (function signatures)
  eenv <- getEenv
  mapWithKeyM (\fun _ -> checkFunSig fun) eenv -- TODO: Map over keys

  --  4 - Function declaration
{-  venv1 <- getVenv
  cenv <- getTenv
  let venv2 = Map.union venv1 cenv
  setVenv venv2
-}
  -- new venv2 as arg
  venv2 <- getVenv
  mapWithKeyM (\fun (a, e) -> checkFD venv2 fun a e) eenv

  venv <- getVenv
  Trav.mapM (\(TypeScheme _ _ t) -> checkUn t) venv
  -- 1 - Check main fun
  checkMainFun


-- | FUNCTIONS TO VERIFY DATATYPES
{- | Checks all the datatypes definitions:
   |  - Checks if they are well kinded and if they have a functional kind
-}
checkDataDecl :: FreestState ()
checkDataDecl = do 
  kenv <- getKenv
  mapM_ checkFunctionalKind kenv
  tenv <- getTenv
  -- mapM_ K.kinding cenv
  mapM_ K.kinding tenv
--  mapWithKeyM (\(KBind p x k) t -> addToKenv (Bind p x) k >> K.kinding t) tenv
--  mapWithKeyM (\(KBind p x k) t -> addToKenv (Bind p x) k >> K.kinding t) tenv
  return ()

checkFunctionalKind :: Kind -> FreestState ()
checkFunctionalKind k =
  when (k < Kind (position k) Functional Un) $
    addError (position k) ["Expecting a functional (TU or TL) type; found a",
                           styleRed (show k), "type."]

-- | FUNCTIONS TO VERIFY FUNCTION SIGNATURES

-- | Verifies if a function exists and if it is well kinded
checkFunSig :: Bind -> FreestState ()
checkFunSig b = do  
  t <- checkFun b
  K.kinding t
  return ()

checkFun :: Bind -> FreestState TypeScheme
checkFun b@(Bind pos x) = do
  getFromVenv b >>= \case
    Just t -> return t
    Nothing -> do
      addError pos ["Function", styleRed ("'" ++ x ++ "'"), "not in scope"]
      let t = (TypeScheme pos [] (Basic pos UnitType))
      addToVenv b t
      return t

-- | FUNCTION DECLARATION

{- | Checks a function declaration:
   |  - Checks the function form
   |  - Checks the function body (expression) against the declared type
-}
checkFD ::  VarEnv -> Bind -> [Bind] -> Expression -> FreestState ()
checkFD venv fname bs exp = do
  checkFunForm venv fname bs
  let t = venv Map.! fname
  let (TypeScheme _ _ lt) = last $ toList t
  checkAgainst exp lt
  return ()

{- | Checks the form of one function:
   | - Checks if a function is applied to the correct number of arguments
   | - Adds each argument and its own type to the environment
-}
checkFunForm :: VarEnv -> Bind -> [Bind] -> FreestState ()
checkFunForm venv fun args = do
  let t = venv Map.! fun
  arguments <- checkArgs fun args (init (toList t))
  foldM (\acc (b@(Bind p _), t) -> addToVenv b t) () arguments
  return ()

checkArgs :: Bind -> [Bind] -> [TypeScheme] -> FreestState [(Bind, TypeScheme)]
checkArgs (Bind p c) ps ts 
  | length ps == length ts = return $ zip ps ts
  | length ps > length ts = do
      addError p ["Function or constructor", "'" ++ styleRed c ++ "'",
                  "is applied to too many arguments"]
      return []
  | length ps < length ts = do
      addError p ["Function or constructor", "'" ++ styleRed c ++ "'",
                  "is applied to too few arguments"]
      return []

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)

-- | FUNCTIONS TO VERIFY THAT THE MAIN FUNCTION EXISTS

checkMainFun :: FreestState ()
checkMainFun = do
  venv <- getVenv
  when ((Bind defaultPos "main") `Map.notMember` venv) $
    addError (defaultPos) ["The IO action", styleRed "'main'", "is not defined"]    

