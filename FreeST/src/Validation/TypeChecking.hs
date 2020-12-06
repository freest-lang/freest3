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
  )
where

import           Syntax.Expression
import           Syntax.Schemes
import           Syntax.Type
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Validation.Kinding            as K
import qualified Validation.Typing             as T
import           Utils.FreestState
import           Utils.PreludeLoader            ( userDefined )
import           Control.Monad.State            ( when
                                                , get
                                                , unless
                                                )
import qualified Data.Map.Strict               as Map

typeCheck :: FreestState ()
typeCheck = do
--  tEnv <- getTEnv -- Type/datatype declarations
  vEnv <- getVEnv -- Function signatures
  eEnv <- getEEnv -- Function bodies
--  tn   <- getTypeNames -- Type Names
  -- debugM ("\n\n\nEntering type checking with\n  TEnv " ++ show tEnv ++ "\n\n"
  --         ++ "  VEnv " ++ show (userDefined vEnv) ++ "\n\n"
  --         ++ "  EEnv " ++ show eEnv  ++ "\n\n"
  --         ++ "  Tname " ++ show tn)

  -- * Check the formation of all type decls
--  debugM "checking the formation of all type decls"
--  mapM_ (K.synthetise Map.empty . snd) tEnv

  -- * Check the formation of all function signatures
--  debugM "checking the formation of all function signatures (kinding)" 
  mapM_ (K.synthetise Map.empty) vEnv
  -- Gets the state and only continues if there are no errors so far
  -- Can't continue to equivalence if there are ill-formed types
  -- (i.e. not contractive under a certain variable)  
  s <- get
  unless (hasErrors s) $ do
    -- * Check whether all function signatures have a binding
--    debugM "checking whether all function signatures have a binding"
    tMapWithKeyM_ checkHasBinding vEnv
    -- * Check function bodies
--    debugM "checking the formation of all functions (typing)"
    tMapWithKeyM_ checkFunBody    eEnv
    -- * Check the main function
--    debugM "checking the main function"
    checkMainFunction

-- Check whether a given function signature has a corresponding
-- binding. Exclude the builtin functions and the datatype
-- constructors.
checkHasBinding :: ProgVar -> Type -> FreestState ()
checkHasBinding f _ = do
  eEnv <- getEEnv
  vEnv <- getVEnv
  tEnv <- getTEnv
  when
      (               f
      `Map.member`    userDefined (noConstructors tEnv vEnv)
      &&              f
      `Map.notMember` eEnv
      )
    $ addError
        (pos f)
        [ Error "The type signature for"
        , Error f
        , Error "lacks an accompanying binding\n"
        , Error "\t Type signature:"
        , Error $ vEnv Map.! f
        ]

-- Check a given function body against its type; make sure all linear
-- variables are used.
checkFunBody :: ProgVar -> Exp -> FreestState ()
checkFunBody f e = getFromVEnv f >>= \case
  Just s  -> T.checkAgainst Map.empty e s
  Nothing -> return () -- We've issued this error at parsing time

checkMainFunction :: FreestState ()
checkMainFunction = do
  let main = mkVar defaultPos "main"
  vEnv <- getVEnv
  if main `Map.notMember` vEnv
    then addError defaultPos
                  [Error "Function", Error main, Error "is not defined"]
    else do
      let s = vEnv Map.! main
--      tEnv <- getTEnv
      unless (isValidMainType s) $ K.synthetise Map.empty s >>= \k -> addError
        defaultPos
        [ Error "The type of"
        , Error main
        , Error "must be non-function, non-polymorphic\n"
        , Error "\t found type (scheme)"
        , Error s
        , Error "of kind"
        , Error k
        ]

isValidMainType :: Type -> Bool
isValidMainType Forall{} = False
isValidMainType Fun{}    = False
isValidMainType _        = True
