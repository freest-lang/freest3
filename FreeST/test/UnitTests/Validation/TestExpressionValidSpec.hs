module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        
import           Elaboration.Duality        
import           SpecUtils
import           Syntax.Expression
import           Util.FreestState
import           Util.PreludeLoader            ( prelude )
import           Validation.Typing              ( checkAgainst )
import Debug.Trace

spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  mapM_ matchValidExpressionSpec (chunksOf 2 t)


matchValidExpressionSpec :: [String] -> Spec
matchValidExpressionSpec [e, t] =
  it (e ++ " : " ++ t) $
    isExpr (read e) (read t) `shouldBe` Left True

isExpr :: Exp -> Type -> TestExpectation
isExpr e t = testValidExpectation True (errors s) -- null (errors s)
 where
  s    = execState test
         (initialState "Check Against Expression") { varEnv = prelude }
  test = join $ checkAgainst Map.empty <$> resolveDualof e <*> resolveDualof t
