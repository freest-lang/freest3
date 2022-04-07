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

{-# LANGUAGE LambdaCase #-}

module Validation.TypeChecking
  ( typeCheck
  )
where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( noConstructors )
import qualified Syntax.Type as T
import           Util.FreestState
import           Util.Error
import           Util.PreludeLoader ( userDefined )
import qualified Validation.Kinding as K
import qualified Validation.Typing as Typing -- Again


import           Control.Monad.State ( when, get, unless )
import qualified Data.Map.Strict as Map
import           Prelude hiding (span)

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
--  debugM "checking the formation of all type decls"
  mapM_ (K.synthetise Map.empty . snd) =<< getTEnv

  -- * Check the formation of all function signatures
--  debugM "checking the formation of all function signatures (kinding)"
  mapM_ (K.synthetise Map.empty) =<< getVEnv
  -- Gets the state and only continues if there are no errors so far
  -- Can't continue to equivalence if there are ill-formed types
  -- (i.e. not contractive under a certain variable)
  s <- get
  unless (hasErrors s) $ do
    -- * Check whether all function signatures have a binding
--    debugM "checking whether all function signatures have a binding"
    tMapWithKeyM_ checkHasBinding =<< getVEnv
    -- * Check function bodies
--    debugM "checking the formation of all functions (typing)"
    tMapWithKeyM_ checkFunBody =<< getProg
    -- * Check the main function
--    debugM "checking the main function"
    checkMainFunction

-- Check whether a given function signature has a corresponding
-- binding. Exclude the builtin functions and the datatype
-- constructors.
checkHasBinding :: Variable -> T.Type -> FreestState ()
checkHasBinding f _ = do
  eEnv <- getProg
  vEnv <- getVEnv
  tEnv <- getTEnv
  when (f `Map.member` userDefined (noConstructors tEnv vEnv) &&
        f `Map.notMember` eEnv )
    $ addError (SignatureLacksBinding (span f) f (vEnv Map.! f))

-- Check a given function body against its type; make sure all linear
-- variables are used.
checkFunBody :: Variable -> E.Exp -> FreestState ()
checkFunBody f e = getFromVEnv f >>= \case
  Just s  -> Typing.checkAgainst Map.empty e s
  Nothing -> return ()

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
        let sp = span $ fst $ Map.elemAt (Map.findIndex main vEnv) vEnv in
        addError (UnrestrictedMainFun sp main t k)
    else when (isMainFlagSet runOpts) $
      addError (MainNotDefined (defaultSpan {defModule = runFilePath runOpts}) main)

-- FIXME: defaultSpan ??


-- validMainType :: T.Type -> Bool -- TODO: why this restriction?
-- validMainType T.Forall{} = False
-- validMainType T.Fun{}    = False
-- validMainType _          = True
