module Types.TestEquivalenceValidSpec(spec) where


import SpecHelper
import Types.TypeEquivalence

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/Types/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ do
      mapM_ matchValidSpec (convert t)


matchValidSpec :: (String, String) -> Spec
matchValidSpec (a, b) =
  it (a ++ " `equivalent` " ++  b) $ do
    ((read a :: Type) `equivalent` (read b :: Type)) `shouldBe` True
