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

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.ProgramVariables
import           Syntax.Base
import           Syntax.Show
import           Equivalence.Normalisation
import           Validation.Extract
import qualified Validation.Kinding as K
import qualified Validation.Typing as T
import           Utils.Errors
import           Utils.FreestState
import           Utils.PreludeLoader (userDefined)
import           Control.Monad.State (when)
import qualified Data.Map.Strict as Map
import           Debug.Trace

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations: check TypeEnv for type or datatype
  -- declarations, VarEnv for each datatype constructor
  tEnv <- getTEnv
  vEnv <- getVEnv
  eEnv <- getEEnv
  trace ("Entering type checking\nTEnv " ++ show tEnv)
    trace ("VEnv " ++ show (userDefined vEnv))
      trace ("EEnv " ++ show eEnv)
        return ()
  tEnv <- getTEnv
  mapM_ (K.synthetiseTS Map.empty . snd) tEnv -- check the formation of all type schemes
  -- Function signatures (VarEnv)
  vEnv <- getVEnv
  mapM_ (K.synthetiseTS Map.empty) vEnv
  tMapWithKeyM checkHasBinding vEnv
  -- Function bodies (ExpEnv)
  eEnv <- getEEnv
  tMapWithKeyM checkFunBody eEnv
  -- Main function
  checkMainFunction

-- Check whether all functions signatures have a binding. Exclude the
-- builtin functions and the datatype constructors.
checkHasBinding :: ProgVar -> TypeScheme -> FreestState ()
checkHasBinding f _ = do
  eEnv <- getEEnv
  vEnv <- getVEnv
  tEnv <- getTEnv
  when (f `Map.member` (userDefined (noConstructors tEnv vEnv)) &&
        f `Map.notMember` eEnv) $
    addError (position f) ["The type signature for", styleRed $ show f,
                           "lacks an accompanying binding"]

checkFunBody :: ProgVar -> Expression -> FreestState ()
checkFunBody f e =
  getFromVEnv f >>= \case
    Just s  -> T.checkAgainstTS e s
    Nothing -> return () -- We've issued this error at parsing time

checkMainFunction :: FreestState ()
checkMainFunction = do
  let main = mkVar defaultPos "main"
  vEnv <- getVEnv
  if main `Map.notMember` vEnv
  then
    addError defaultPos ["Function", styleRed "main", "is not defined"]
  else do
    let s = vEnv Map.! main
    tEnv <- getTEnv
    when (not (isValidMainType s)) $
      K.synthetiseTS Map.empty s >>= \k ->
      addError defaultPos ["The type of", styleRed "main", "must be non-function, non-polymorphic\n",
                           "\t found type (scheme)", styleRed $ show s, "of kind", styleRed $ show k]

isValidMainType :: TypeScheme -> Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = False
isValidMainType (TypeScheme _ [] _)            = True
isValidMainType _                              = False
