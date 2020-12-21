module Validation.TestTypesValidSpec
  ( spec
  )
where

import           Control.Monad.State            ( runState )
import qualified Data.Map.Strict               as Map
                                                ( empty )
import           Elaboration.Elaboration
import           SpecHelper
import           Syntax.Kind                    ( Kind )
import           Utils.FreestState              ( initialState
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
matchValidKindingSpec [t, k] = it t $ hasKind (read t) (read k) `shouldBe` True

hasKind :: Type -> Kind -> Bool
hasKind t k = null (errors s) && k' <: k
 where
--  t'      = renameType t
  (k', s) = runState test (initialState "Kind synthesis")

  test    = synthetise Map.empty . renameType =<< elaborate t
  -- test = do
  --   t' <- subsType Map.empty Nothing t
  --   synthetise Map.empty (renameType t')


main :: IO ()
main = hspec spec
