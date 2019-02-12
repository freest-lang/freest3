module Validation.TestTypeSchemeKindingSpec(spec) where

import qualified Data.Map as Map
import           Parse.Parser
import           SpecHelper
import           Validation.Kinding
import           Syntax.Kinds

spec :: Spec
spec = do 
  describe "Valid Kinding TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypeSchemeKindingValid.txt"
    mapM_ matchKindingValidSpec (chunksOf 2 t)

matchKindingValidSpec :: [String] -> Spec
matchKindingValidSpec [ts, k] =
  it ts $ do
    (kindOfScheme (0,0) (read ts :: TypeScheme)) `shouldBe` (read k :: Kind)

main :: IO ()
main = hspec spec
