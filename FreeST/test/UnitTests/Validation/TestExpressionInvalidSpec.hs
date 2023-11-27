module Validation.TestExpressionInvalidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.ResolveDuality    as Dual
import           SpecUtils
import           Syntax.Expression
import           Validation.Typing              ( synthetise )
import           Util.State
import qualified Elaboration.Phase as EP
import qualified Validation.Phase as VP
import FreeST

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
  (e',s0) = runState (Dual.resolve e) EP.initialElab
  s = execState (synthetise Map.empty e') (VP.initialTyp defaultOpts){errors=errors s0}

main :: IO ()
main = hspec spec
