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
      mapM_ matchInvalidSpec (convert t)


matchInvalidSpec :: (String, String) -> Spec
matchInvalidSpec (a, b) =
  it (a ++ " `equivalent` " ++  b) $ do
    let equiv = ((read a :: Type) `equivalent` (read b :: Type))
    equiv `shouldBe` False
