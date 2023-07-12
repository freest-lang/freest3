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

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Util.Error
import           Util.State
import qualified Validation.Kinding as K
import           Validation.Phase hiding (Typing)
import qualified Validation.Typing as Typing -- Again

import           Control.Monad
import           Control.Monad.State ( get )
import qualified Data.Map.Strict as Map

typeCheck :: RunOpts -> TypingState ()
typeCheck runOpts = do
  -- * Check the formation of all type decls
  mapM_ (uncurry $ K.checkAgainst Map.empty) =<< getTypes
  -- * Check the formation of all function signatures
  mapM_ (K.synthetise Map.empty) =<< getSignatures
  -- Gets the state and only continues if there are no errors so far
  s <- get
  unless (hasErrors s) $ do
    -- * Check function bodies
    tMapWithKeyM_ (checkFunBody (signatures $ ast s)) =<< getDefs
    -- * Check the main function
    checkMainFunction runOpts
    -- * Checking final environment for linearity
    checkLinearity

-- Check a given function body against its type; make sure all linear
-- variables are used.
checkFunBody :: Signatures -> Variable -> E.Exp -> TypingState ()
checkFunBody sigs f e =
  forM_ (sigs Map.!? f) (Typing.checkAgainst Map.empty e)

checkMainFunction :: RunOpts -> TypingState ()
checkMainFunction runOpts = do
  sigs <- getSignatures
  let main = getMain runOpts
  if main `Map.member` sigs
    then do
      let t = sigs Map.! main
      k <- K.synthetise Map.empty t
      when (K.isLin k) $
        let sp = getSpan $ fst $ Map.elemAt (Map.findIndex main sigs) sigs in
        addError (UnrestrictedMainFun sp main t k)
    else when (isMainFlagSet runOpts) $
      addError (MainNotDefined (defaultSpan {defModule = runFilePath runOpts}) main)

checkLinearity :: TypingState ()
checkLinearity = do
  sigs <- getSignatures
  m <- filterM (K.lin . snd) (Map.toList sigs)
  unless (null m) $ addError (LinearFunctionNotConsumed (getSpan (fst $ head m)) m) 

