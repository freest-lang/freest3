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

module Validation.TypeChecking
( typeCheck
) where

import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Bind
import           Equivalence.Normalisation
import qualified Validation.Kinding as K
import qualified Validation.Typing as T
import           Parse.Lexer (position, defaultPos)
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
  mapM_ (K.synthetiseTS Map.empty . snd) tenv -- check the formation of all type schemes
  -- Function signatures (VarEnv)
  venv <- getVenv
  mapM_ (K.synthetiseTS Map.empty) venv
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

checkFunBody :: PBind -> Expression -> FreestState ()
checkFunBody f e =
  getFromVenv f >>= \case
    Just s ->
      T.checkAgainstTS e s
    Nothing ->
      return () -- We've issued this error at parsing time

checkMainFunction :: FreestState ()
checkMainFunction = do
  venv <- getVenv
  let mBind = PBind defaultPos "main"
  if mBind `Map.notMember` venv
  then
    addError defaultPos ["Function", styleRed "main", "is not defined"]
  else do
    let s = venv Map.! mBind
    tenv <- getTenv
    let mType = normalise tenv s
    b <- isValidMainType mType
    k <- K.synthetiseTS Map.empty s
    when (not b) $
      addError (position mType) ["The type for", styleRed "main", "must be an unrestricted, non-function type\n",
                                 "\t found type (scheme)", styleRed $ show s, "of kind", styleRed $ show k]

isValidMainType :: TypeScheme -> FreestState Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = return False
isValidMainType t@(TypeScheme _ [] _)          = K.un t
isValidMainType _                              = return False
