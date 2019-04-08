module Validation.TestTypeSchemeKindingSpec(spec) where

import           Parse.Parser
import           Validation.Kinding
import           Syntax.Kinds
import           Syntax.Schemes
import           Utils.FreestState
import           SpecHelper
import           Control.Monad.State
import qualified Data.Map as Map

spec :: Spec
spec = do 
  describe "Valid Kinding TypeSchemes" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypeSchemeKindingValid.txt"
    mapM_ matchKindingValidSpec (chunksOf 2 t)

matchKindingValidSpec :: [String] -> Spec
matchKindingValidSpec [ts, k] =
  it ts $ do
    (kindOfScheme (read ts :: TypeScheme)) `shouldBe` (read k :: Kind)

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (synthetiseTS Map.empty t) (initialState "")

main :: IO ()
main = hspec spec
