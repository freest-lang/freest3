module Validation.TestExpressionInvalidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration -- ( elaborate )
import           SpecHelper
import           Syntax.Expression
import           Utils.FreestState
import           Validation.Typing              ( synthetise )

spec :: Spec
spec = describe "Invalid expression tests" $ do
  e <- runIO
    $ readFromFile "test/UnitTests/Validation/TestExpressionInvalid.txt"
  mapM_ matchInvalidExpSpec e

matchInvalidExpSpec :: String -> Spec
matchInvalidExpSpec e = it e $ isExpr (read e) `shouldBe` False

isExpr :: Exp -> Bool
isExpr e = null (errors s)
 where
  s = execState (synthetise Map.empty =<< elaborate e)
                (initialState "Check Against Expression") -- (elaborateExp e)


main :: IO ()
main = hspec spec


