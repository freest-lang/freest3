module Types.TestEquivalenceValidSpec(spec) where


import qualified Data.Map.Strict as Map
import           SpecHelper
import           Types.TypeEquivalence

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ do
      mapM_ matchValidSpec (convert t)


matchValidSpec :: (String, String) -> Spec
matchValidSpec (a, b) =
  it (a ++ " `equivalent` " ++  b) $ do
    let equiv = equivalent Map.empty (read a :: Type) (read b :: Type)
    equiv `shouldBe` True
