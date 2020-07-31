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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring, BangPatterns #-}

module Validation.TypeChecking
( typeCheck
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.ProgramVariables
import           Syntax.Base
import           Validation.Contractive
import qualified Validation.Kinding as K
import qualified Validation.Typing as T
import           Utils.Errors
import           Utils.FreestState
import           Utils.PreludeLoader (userDefined)
import           Control.Monad.State (when, get, unless)
import qualified Data.Map.Strict as Map
-- import           Syntax.Show -- debug
-- import           Debug.Trace -- debug

typeCheck :: FreestState ()
typeCheck = do
  tEnv <- getTEnv -- Type/datatype declarations
  vEnv <- getVEnv -- Function signatures
  eEnv <- getEEnv -- Function bodies
  tn <- getTypeNames -- Type Names
  -- traceM ("\n\n\nEntering type checking with\n  TEnv " ++ show tEnv ++ "\n\n" ++
  --         "  VEnv " ++ show (userDefined vEnv) ++ "\n\n" ++
  --         "  EEnv " ++ show eEnv  ++ "\n\n" ++
  --         "  Tname " ++ show tn)

  -- 1. Check the contractiveness of all type decls
  -- trace "checking contractiveness of all type decls" (return ())
  -- mapM_ (checkContractive Map.empty . snd) tEnv

  -- 2. Check the formation of all type decls
  -- trace "checking the formation of all type decls" (return ())
  mapM_ (K.synthetiseTS Map.empty . snd) tEnv
  -- 3. Check the formation of all function signatures
  -- trace "checking the formation of all function signatures (kinding)" (return ())
  mapM_ (K.synthetiseTS Map.empty) vEnv
  -- Gets the state and only continues if there are no errors so far
  -- Can't continue to equivalence if there are ill-formed types
  -- (i.e. not contractive under a certain variable)  
  get >>= \s -> unless (hasErrors s) $ do
    -- 4. Check whether all function signatures have a binding
    -- trace "checking whether all function signatures have a binding" (return ())
    tMapWithKeyM checkHasBinding vEnv
    -- 5. Check function bodies
    -- trace "checking the formation of all functions (typing)" (return ())
    tMapWithKeyM checkFunBody eEnv
    -- 6. Check the main function
    -- trace "checking the main function" (return ())
    checkMainFunction

-- Check whether a given function signature has a corresponding
-- binding. Exclude the builtin functions and the datatype
-- constructors.
checkHasBinding :: ProgVar -> TypeScheme -> FreestState ()
checkHasBinding f _ = do
  eEnv <- getEEnv
  vEnv <- getVEnv
  tEnv <- getTEnv
  when (f `Map.member` (userDefined (noConstructors tEnv vEnv)) &&
        f `Map.notMember` eEnv) $
    addError (position f) [Error "The type signature for", Error f,
                           Error "lacks an accompanying binding\n",
                           Error "\t Type signature:", Error $ vEnv Map.! f]

-- Check a given function body against its type; make sure all linear
-- variables are used.
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
    addError defaultPos [Error "Function", Error main, Error "is not defined"]
  else do
    let s = vEnv Map.! main
    tEnv <- getTEnv
    when (not (isValidMainType s)) $
      K.synthetiseTS Map.empty s >>= \k ->
      addError defaultPos [Error "The type of", Error main, Error "must be non-function, non-polymorphic\n",
                           Error "\t found type (scheme)", Error s, Error "of kind", Error k]

isValidMainType :: TypeScheme -> Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = False
isValidMainType (TypeScheme _ [] _)            = True
isValidMainType _                              = False
