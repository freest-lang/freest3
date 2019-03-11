module Parse.TestParserInvalidSpec(spec) where

import SpecHelper
import Control.Exception (evaluate)
import Test.Hspec.Expectations (anyException, shouldThrow)
-- import qualified Data.Map.Strict as Map


-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Invalid parser tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Parse/TestParserTypesInvalid.txt"
    runIO $ putStrLn $ show t
    mapM_ matchInvalidTypesSpec t

matchInvalidTypesSpec :: String -> Spec
matchInvalidTypesSpec t =
  it t $ do
    evaluate (read t :: Type) `shouldThrow` anyException


-- -- pendingWith "need to make this set of tests"

-- out and in of a type that isn't basic

