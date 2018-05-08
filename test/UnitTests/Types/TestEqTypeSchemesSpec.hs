module Types.TestEqTypeSchemesSpec(spec) where

import qualified Data.Map as Map
import           SpecHelper
import           Terms.Parser
import           Types.Kinds
import           Types.Kinding


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Equal TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestEqTypeSchemeValid.txt"
    mapM_ (matchEqSpec True) (convert t) 

  describe "Not equal TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestEqTypeSchemeInvalid.txt"
    mapM_ (matchEqSpec False) (convert t)

  describe "Valid Show TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestShowTypeScheme.txt"
    mapM_ matchShowSpec t
 
  describe "Valid Kinding TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestTypeSchemeKindingValid.txt"
    mapM_ matchKindingValidSpec (convert t)
 

matchEqSpec :: Bool -> (String, String) -> Spec
matchEqSpec b (ts1, ts2) =
  it (ts1 ++ " == " ++ ts2) $ do
    (read ts1 :: TypeScheme) == (read ts2 :: TypeScheme) `shouldBe` b


matchShowSpec :: String -> Spec
matchShowSpec a =
  it a $ do
    (read (show(read a :: TypeScheme)) :: TypeScheme) `shouldBe` (read a :: TypeScheme)


matchKindingValidSpec :: (String, String) -> Spec
matchKindingValidSpec (ts, k) =
  it ts $ do
    (kindOfScheme Map.empty (read ts :: TypeScheme)) `shouldBe` (read k :: Kind)

