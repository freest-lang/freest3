module Types.TestEquivalenceInvalidSpec(spec) where


import SpecHelper
import Types.TypeEquivalence

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/Types/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ do
      mapM_ matchValidSpec (convert t)


matchValidSpec :: (String, String) -> Spec
matchValidSpec (a, b) =
  it (a ++ " `equivalent` " ++  b) $ do
    ((read a :: Type) `equivalent` (read b :: Type)) `shouldBe` False
