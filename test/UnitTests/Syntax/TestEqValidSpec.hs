module Syntax.TestEqValidSpec(spec) where

import SpecHelper

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Syntax/TestEqValid.txt"
  describe "Valid Equality Test" $
      mapM_ matchValidSpec (chunksOf 2 t)

matchValidSpec :: [String] -> Spec
matchValidSpec [t, u] =
  it (t ++ " == " ++  u) $
    (read t :: Type) == (read u :: Type) `shouldBe` True

main :: IO ()
main = hspec spec
