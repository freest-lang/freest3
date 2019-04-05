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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Typing
( typeCheck
) where

import           Parse.Lexer (position, defaultPos)
import           Syntax.Expression (Expression(..))
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Bind
import           Syntax.Programs
import qualified Validation.Kinding as K
import qualified Validation.TypingExps as T
import           Utils.Errors
import           Utils.FreestState
import           Validation.Extract
import           Utils.PreludeLoader (isBuiltin)
import           Control.Monad.State (when)
import qualified Data.Map.Strict as Map
import           Debug.Trace

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations: check TypeEnv for type or datatype
  -- declarations, VarEnv for each datatype constructor
  tenv <- getTenv
  mapM_ (K.synthetiseTS . snd) tenv -- check the formation of all type schemes
  -- Function signatures (VarEnv)
  venv <- getVenv
  mapM_ K.synthetiseTS venv
  tMapWithKeyM hasBinding venv
  -- Function bodies (ExpEnv)
  eenv <- getEenv
  tMapWithKeyM checkFunBody eenv
  -- Main function
  checkMainFunction

-- Check whether all functions signatures have a binding. Exclude the
-- builtin functions and the datatype constructors.
hasBinding :: PBind -> a -> FreestState ()
hasBinding f _ = do
  eenv <- getEenv
  venv <- getVenv
  tenv <- getTenv
  when (f `Map.member` (T.funSigsOnly tenv venv) && f `Map.notMember` eenv) $
    addError (position f) ["The type signature for", styleRed $ show f,
                           "lacks an accompanying binding"]

-- Check whether there is a signature for a given function. But first,
-- and for type checking purposes only, replace the dummy Unit types
-- in the bodies of functions.
checkFunBody :: PBind -> Expression -> FreestState ()
checkFunBody f e =
  getFromVenv f >>= \case
    Just ts -> do
      t <- T.fillFunType Map.empty f e ts -- TODO: what to do with t?
      return ()
    Nothing ->
      addError (position f) ["Did not find the signature of function", styleRed $ show f]

checkMainFunction :: FreestState ()
checkMainFunction = do
  venv <- getVenv
  let mBind = PBind defaultPos "main"
  if mBind `Map.notMember` venv
  then
    addError defaultPos [styleRed "main", "is not defined"]
  else do
    let t = venv Map.! mBind
    mType <- normaliseTS t
    b <- isValidMainType mType
    k <- K.synthetiseTS t
    when (not b) $
      addError (position mType) ["The type for", styleRed "main", "must be an unrestricted, non-function type\n",
                                 "\t found type", styleRed $ show t, ":", styleRed $ show k]

isValidMainType :: TypeScheme -> FreestState Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = return False
isValidMainType t@(TypeScheme _ [] _)          = K.un t
isValidMainType _                              = return False
