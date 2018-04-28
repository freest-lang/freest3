module Types.TestEqValidSpec(spec) where

import SpecHelper

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestEqValid.txt"
  describe "Valid Equality Test" $ do
      mapM_ matchValidSpec (convert t)

matchValidSpec :: (String, String) -> Spec
matchValidSpec (t, u) =
  it (t ++ " == " ++  u) $ do
    ((read t :: Type) == (read u :: Type)) `shouldBe` True

main :: IO ()
main = hspec spec
