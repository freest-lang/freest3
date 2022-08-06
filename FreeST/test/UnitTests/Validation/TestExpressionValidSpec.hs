module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Elaboration.ResolveDuality as Dual
import           SpecUtils
import           Syntax.Expression
import           Util.FreestState
import           Syntax.Program (noConstructors)
-- import           Util.PreludeLoader         ( prelude )
import           Validation.Rename
import           Validation.Typing          ( checkAgainst )
import           Paths_FreeST ( getDataFileName )
-- import           Util.CmdLine
import           Util.FreestState
import           Elaboration.Elaboration ( elaboration )




spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  p <- runIO prelude
  mapM_ (matchValidExpressionSpec p) (chunksOf 2 t)


matchValidExpressionSpec :: FreestS -> [String] -> Spec
matchValidExpressionSpec p [e, t] =
  it (e ++ " : " ++ t) $
    isExpr p (read e) (read t) `shouldBe` Left True

isExpr :: FreestS -> Exp -> Type -> TestExpectation
isExpr prelude e t = testValidExpectation True (errors s) -- null (errors s)
 where
  s    = execState test prelude  -- { varEnv = prelude }
  test = do
    t' <- rename Map.empty Map.empty =<< Dual.resolve t
    e' <- rename Map.empty Map.empty =<< Dual.resolve e
    checkAgainst Map.empty e' t'

prelude = do
  preludeFp <- getDataFileName "Prelude.fst"
  let s0 = initialState {runOpts=defaultOpts{runFilePath=preludeFp}}
  s1 <- parseProgram s0
  -- | Prelude entries without body are builtins  
  let venv = Map.keysSet (noConstructors (typeEnv s1) (varEnv s1))
  let penv = Map.keysSet (parseEnv s1)
  let bs = Set.difference venv penv
  let s2 = emptyPEnv $ execState elaboration s1
  let s3 = execState renameState s2
  return $ s3 { builtins = bs }
