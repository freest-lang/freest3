module Syntax.TestEqInvalidSpec(spec) where

import SpecHelper

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Syntax/TestEqInvalid.txt"
  describe "Invalid Equality Test" $ do
      mapM_ matchInvalidSpec (chunksOf 2 t)

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [t, u] =
  it (t ++ " == " ++  u) $ do
    ((read t :: Type) == (read u :: Type)) `shouldBe` False

main :: IO ()
main = hspec spec
