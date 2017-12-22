module Types.ParseSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Types.TestParse
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
--import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "some legacy HUnit tests" $ do
    fromHUnitTest allTests
