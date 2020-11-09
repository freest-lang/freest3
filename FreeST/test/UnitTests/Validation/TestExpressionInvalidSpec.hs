module Validation.TestExpressionInvalidSpec(spec) where

import           SpecHelper
import           Syntax.Expressions
import           Control.Monad.State
import           Utils.FreestState
import           Validation.Typing(synthetise)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Invalid expression tests" $ do
    e <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionInvalid.txt"
    mapM_ matchInvalidExpressionSpec e

matchInvalidExpressionSpec :: String -> Spec
matchInvalidExpressionSpec e =
  it e $ isExpr (read e) `shouldBe` False

isExpr :: Expression -> Bool
isExpr e = null (errors s)
  where s = execState (synthetise Map.empty e) (initialState "Check Against Expression")


main :: IO ()
main = hspec spec


