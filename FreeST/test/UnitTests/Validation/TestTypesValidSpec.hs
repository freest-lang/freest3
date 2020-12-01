module Validation.TestTypesValidSpec
  ( spec
  )
where

import           Syntax.Kind                    ( Kind
                                                , (<:)
                                                )
import           Validation.Rename              ( renameType )
import           Validation.Kinding             ( synthetise )
import           Utils.FreestState              ( initialState
                                                , errors
                                                )
import           Control.Monad.State            ( runState )
import qualified Data.Map.Strict               as Map
                                                ( empty )
import           SpecHelper

spec :: Spec
spec = describe "Valid type tests" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesValid.txt"
  mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [t, k] = it t $ hasKind (read t) (read k) `shouldBe` True

hasKind :: Type -> Kind -> Bool
hasKind t k | null (errors s) = k' <: k
            | otherwise       = False
 where
  t'      = renameType t
  (k', s) = runState (synthetise Map.empty t') (initialState "Kind synthesis")

main :: IO ()
main = hspec spec
