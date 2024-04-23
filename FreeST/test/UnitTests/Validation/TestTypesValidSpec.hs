module Validation.TestTypesValidSpec
  ( spec
  )
where

import           Syntax.Kind ( Kind )
import           Elaboration.ResolveDuality as Dual
import           Elaboration.Phase
import           Typing.Rename ( renameType )
import           Kinding.Subkind ( (<:) )
import           Kinding.Kinding ( synthetise )
import           Util.State ( initialS, errors )
import           SpecUtils

import           Control.Monad.State ( runState )
import qualified Data.Map.Strict as Map ( empty )

spec :: Spec
spec = describe "Valid type tests" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesValid.txt"
  mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [t, k] = it t $ hasKind (read t) (read k) `shouldBe` Left True

hasKind :: Type -> Kind -> TestExpectation
hasKind t k = testValidExpectation (k' <: k) (errors s) -- null (errors s) && k' <: k
 where
  (k', s) = runState test initialElab
  test    = synthetise Map.empty . renameType =<< Dual.resolve t
  
main :: IO ()
main = hspec spec
