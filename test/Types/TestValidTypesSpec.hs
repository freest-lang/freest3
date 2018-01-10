module Types.TestValidTypesSpec(spec) where

-- import System.IO
-- import Control.Monad
-- import Data.List
-- import Data.Char
import SpecHelper
-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/Types/TestValidTypes.txt"
  describe "Valid Equality Test" $ do
      mapM_ matchValidSpec (convert t)


matchValidSpec :: (String, String) -> Spec
matchValidSpec (a, b) =
  it (a ++ " == " ++  b) $ do
    ((read a :: Type) == (read b :: Type)) `shouldBe` True
