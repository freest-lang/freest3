module Syntax.TestEqTypeSchemeSpec(spec) where

import qualified Data.Map as Map
import           Parse.Parser
import           SpecHelper
import           Validation.Kinding
import           Syntax.Kinds

spec :: Spec
spec = do
  describe "Equal TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Syntax/TestEqTypeSchemeValid.txt"
    mapM_ (matchEqSpec True) (chunksOf 2 t) 

  describe "Not equal TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Syntax/TestEqTypeSchemeInvalid.txt"
    mapM_ (matchEqSpec False) (chunksOf 2 t)

  describe "Valid Show TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Syntax/TestShowTypeScheme.txt"
    mapM_ matchShowSpec t
 
  --describe "Valid Kinding TypeSchemes" $ do
  --  t <- runIO $ readFromFile "test/UnitTests/Syntax/TestTypeSchemeKindingValid.txt"
  --  mapM_ matchKindingValidSpec (chunksOf 2 t)
 

matchEqSpec :: Bool -> [String] -> Spec
matchEqSpec b [ts1, ts2] =
  it (ts1 ++ " == " ++ ts2) $ do
    (read ts1 :: TypeScheme) == (read ts2 :: TypeScheme) `shouldBe` b


matchShowSpec :: String -> Spec
matchShowSpec a =
  it a $ do
    (read (show(read a :: TypeScheme)) :: TypeScheme) `shouldBe` (read a :: TypeScheme)


matchKindingValidSpec :: [String] -> Spec
matchKindingValidSpec [ts, k] =
  it ts $ do
    (kindOfScheme Map.empty (read ts :: TypeScheme)) `shouldBe` (read k :: Kind)

main :: IO ()
main = hspec spec
