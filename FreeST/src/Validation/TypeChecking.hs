{-|

Module      :  Validation.TypeChecking
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.TypeChecking
  ( typeCheck
  )
where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( noConstructors, VarEnv )
import qualified Syntax.Type as T
import           Util.FreestState
import           Util.Error
-- import           Util.PreludeLoader ( userDefined )
import qualified Validation.Kinding as K
import qualified Validation.Typing as Typing -- Again


import           Control.Monad.Extra ( allM, unlessM )
import           Control.Monad.State ( when, get, unless )
import           Control.Monad

import qualified Data.Map.Strict as Map

typeCheck :: FreestState ()
typeCheck = do
  -- vEnv <- getVEnv -- Function signatures
  -- eEnv <- getProg -- Function bodies
  -- tn   <- getTypeNames -- Type Names
  -- tEnv <- getTEnv -- Type/datatype declarations
  -- debugM ("\n\n\nEntering type checking with\n  TEnv " ++ show tEnv ++ "\n\n"
  --         ++ "  VEnv " ++ show (userDefined vEnv) ++ "\n\n"
  --         ++ "  Prog " ++ show eEnv  ++ "\n\n"
  --         ++ "  Tname " ++ show tn)

  -- * Check the formation of all type decls
  mapM_ (uncurry $ K.checkAgainst Map.empty) =<< getTEnv

  -- * Check the formation of all function signatures
  mapM_ (K.synthetise Map.empty) =<< getVEnv
  -- Gets the state and only continues if there are no errors so far
  s <- get
  unless (hasErrors s) $ do
    -- * Check function bodies
    tMapWithKeyM_ (checkFunBody (varEnv s)) =<< getProg
    -- * Check the main function
    checkMainFunction
    -- * Checking final environment for linearity
    checkLinearity

-- Check a given function body against its type; make sure all linear
-- variables are used.
checkFunBody :: VarEnv -> Variable -> E.Exp -> FreestState ()
checkFunBody venv f e =
  forM_ (venv Map.!? f) (Typing.checkAgainst Map.empty e)

checkMainFunction :: FreestState ()
checkMainFunction = do
  vEnv <- getVEnv
  runOpts <- getOpts
  let main = getMain runOpts

  if main `Map.member` vEnv
    then do
      let t = vEnv Map.! main
      k <- K.synthetise Map.empty t
      when (K.isLin k) $
        let sp = getSpan $ fst $ Map.elemAt (Map.findIndex main vEnv) vEnv in
        addError (UnrestrictedMainFun sp main t k)
    else when (isMainFlagSet runOpts) $
      addError (MainNotDefined (defaultSpan {defModule = runFilePath runOpts}) main)

checkLinearity :: FreestState ()
checkLinearity = do
  venv <- getVEnv
  m <- filterM (K.lin . snd) (Map.toList venv)
  unless (null m) $ addError (LinearFunctionNotConsumed (getSpan (fst $ head m)) m) 

