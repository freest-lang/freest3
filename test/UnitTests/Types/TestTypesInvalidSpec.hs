module UnitTests.Types.TestTypesInvalidSpec(spec) where

import SpecHelper

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestTypesInvalid.txt"
  describe "Invalid Equality Test" $ do
      mapM_ matchInvalidSpec (convert t)


matchInvalidSpec :: (String, String) -> Spec
matchInvalidSpec (a, b) =
  it (a ++ " == " ++  b) $ do
    ((read a :: Type) == (read b :: Type)) `shouldBe` False
