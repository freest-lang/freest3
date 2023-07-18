module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Elaboration.Elaboration ( elaboration )
import           Elaboration.ResolveDuality as Dual
import           Paths_FreeST ( getDataFileName )
import           SpecUtils
import           Syntax.Expression
import           Syntax.Program (noConstructors)
import           Util.State
import           Validation.Rename
import           Validation.Typing ( checkAgainst )

import Parse.Phase
import Validation.Phase
import Elaboration.Phase
import PatternMatch.PatternMatch
import FreeST


spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  p <- runIO prelude
  mapM_ (matchValidExpressionSpec p) (chunksOf 2 t)

matchValidExpressionSpec :: (Validation.Phase.Defs, ElabS) -> [String] -> Spec
matchValidExpressionSpec p [e, t] =
  it (e ++ " : " ++ t) $
    isExpr p (read e) (read t) `shouldBe` Left True

isExpr :: (Validation.Phase.Defs, ElabS) -> Exp -> Type -> TestExpectation
isExpr (defs, prelude) e t = testValidExpectation True (errors s) -- null (errors s)
 where
  s    = let ((t',e'), s') = runState resolveBoth prelude in
         execState (test t' e' s') (elabToTyping defaultOpts defs s')

  test t e s' = do
    setErrors (errors s')
    renameState
    t' <- rename Map.empty Map.empty t
    e' <- rename Map.empty Map.empty e
    checkAgainst Map.empty e' t'

  resolveBoth = (,) <$> Dual.resolve t <*> Dual.resolve e

prelude = do
  s0 <- initialWithFile <$> getDataFileName "Prelude.fst"
  -- | Parse
  s2 <-  parseProgram s0 -- parseAndImport s0{extra = (extra s0){runOpts=defaultOpts}} -- | PatternMatch
  -- | PatternMatch
  let patternS = patternMatch s2
  -- | Elaboration
  return $ elaboration patternS
