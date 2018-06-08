module Syntax.TestShowValidSpec(spec) where


import SpecHelper


-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Syntax/TestShowValid.txt"
  describe "Show Test" $ do
      mapM_ matchValidSpec t


matchValidSpec :: String -> Spec
matchValidSpec a =
  it a $ do
    (read (show(read a :: Type)) :: Type) `shouldBe` (read a :: Type)
    --filter (/=' ') (show(read a :: Type)) `shouldBe` filter (/=' ') a
