module Validation.TestTypesValidSpec
  ( spec
  )
where

import           Control.Monad.State            ( runState )
import qualified Data.Map.Strict               as Map
                                                ( empty )
import           Elaboration.Duality           as Dual
import           Elaboration.Elaboration
import           SpecUtils
import           Syntax.Kind                    ( Kind )
import           Util.FreestState               ( initialState
                                                , errors
                                                )
import           Validation.Kinding             ( synthetise )
import           Validation.Rename              ( renameType )
import           Validation.Subkind             ( (<:) )

spec :: Spec
spec = describe "Valid type tests" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesValid.txt"
  mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [t, k] = it t $ hasKind (read t) (read k) `shouldBe` Left True

hasKind :: Type -> Kind -> TestExpectation
hasKind t k = testValidExpectation (k' <: k) (errors s) -- null (errors s) && k' <: k
 where
  (k', s) = runState test (initialState "Kind synthesis")
  test    = synthetise Map.empty . renameType =<< Dual.resolve t
  
main :: IO ()
main = hspec spec
