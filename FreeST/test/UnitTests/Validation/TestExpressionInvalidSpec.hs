module Validation.TestExpressionInvalidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.Duality           as Dual
import           SpecUtils
import           Syntax.Expression
import           Util.FreestState
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
  s = execState (synthetise Map.empty =<< Dual.resolve e) initialState


main :: IO ()
main = hspec spec


