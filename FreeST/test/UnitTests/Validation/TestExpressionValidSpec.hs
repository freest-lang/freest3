module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Syntax.Expression
import           Syntax.Program (noConstructors)
import           Typing.Phase
import           Elaboration.Elaboration ( elaboration )
import           Elaboration.ResolveDuality as Dual
import           Elaboration.Phase
import           Typing.Rename ( renameProgram, rename )
import           Typing.Typing ( checkAgainst )
import           Parse.Phase
import           PatternMatch.PatternMatch
import           Paths_FreeST ( getDataFileName )
import           SpecUtils
import           Util.State
import           FreeST
import           Inference.Inference

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  p <- runIO prelude
  mapM_ (matchValidExpressionSpec p) (chunksOf 2 t)

matchValidExpressionSpec :: (Typing.Phase.Defs, ElabS) -> [String] -> Spec
matchValidExpressionSpec p [e, t] =
  it (e ++ " : " ++ t) $
    isExpr p (read e) (read t) >>= (`shouldBe` Left True)

isExpr :: (Typing.Phase.Defs, ElabS)  -> Exp -> Type -> IO TestExpectation
isExpr (defs, prelude) e t = testValidExpectation True . errors <$> s -- null (errors s)
 where
  s    = let ((t',e'), s') = runState resolveBoth prelude in
         let s'' = execState (renameProgram >> infer) (elabToInf defs s') in
         execStateT (test t' e' s'') (infToTyping defaultOpts s'')

--         (elabToTyping defaultOpts defs s')

  test t e s' = do
    setErrors (errors s')
--    renameProgram
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
  return $ elaboration (Parse.Phase.pkVariables $ extra s2)
                       (Parse.Phase.mVariables $ extra s2) patternS

  -- let (defs, elabS) = elaboration patternS
  -- return $ execState (renameProgram >> infer) (elabToInf defs elabS)
