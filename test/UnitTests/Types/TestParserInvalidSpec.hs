module UnitTests.Types.TestParserInvalidSpec(spec) where

import SpecHelper
import Control.Exception (evaluate)
import Test.Hspec.Expectations (anyException, shouldThrow)
-- import qualified Data.Map.Strict as Map


-- Just to be able to run it alone
main :: IO ()
main = hspec spec

-- pendingWith "need to make this set of tests"
spec :: Spec
spec = do
--  evaluate (read"" :: Type) `shouldThrow` anyException
  describe "Invalid Parser tests" $ do
    it "Nested in out" $ do
      evaluate(read "!? Int" :: Type) `shouldThrow` anyException

    it "Semi Semi" $ do
      evaluate(read "!Int;;?Int" :: Type)  `shouldThrow` anyException

    it "Nested InternalChoice and ExternalChoice" $ do
      evaluate(read "+&{Leaf:Skip,Node:!Int;?Int})" :: Type)  `shouldThrow` anyException

    it "Out skip" $ do
      evaluate (read "!Skip" :: Type) `shouldThrow` anyException

    it "In skip" $ do
      evaluate (read "?Skip" :: Type) `shouldThrow` anyException

    it "Nested Lin Un Fun" $ do
      evaluate (read "!Int ->-o !Int" :: Type) `shouldThrow` anyException

    it "Empty read (no input)" $ do
      evaluate (read "" :: Type) `shouldThrow` anyException

    it "Parens in" $ do
      evaluate (read "(?)Int" :: Type) `shouldThrow` anyException

    it "Parens out" $ do
       evaluate (read "(!)Char" :: Type) `shouldThrow` anyException

    it "Parens LinFun" $ do
      evaluate (read "Char (->) Char" :: Type) `shouldThrow` anyException

    it "Parens UnFun"   $ do
      evaluate (read "Char (->) Char" :: Type) `shouldThrow` anyException

    it "In not basic (skip)" $ do
      evaluate (read "?Skip" :: Type)  `shouldThrow` anyException

    it "Out not basic (skip)" $ do
      evaluate (read "!Skip" :: Type)  `shouldThrow` anyException

-- out and in of a type that isn't basic
