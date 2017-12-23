module Types.KindingSpec(spec) where

import SpecHelper
import Control.Exception (evaluate)
import Types.TestKinding
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Unit tests" $ do
    fromHUnitTest allTests
