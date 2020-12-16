module Equivalence.TestEquivalenceValidSpec (spec) where

import           Syntax.Base
import           Syntax.Kind                   as K
import           Equivalence.Equivalence
import           Validation.Rename
import           Validation.Kinding             ( synthetise )
import           SpecHelper
import           Utils.FreestState              ( initialState
                                                , errors
                                                )
import           Control.Monad.State            ( runState )
import qualified Data.Map.Strict               as Map

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] = it
  (k ++ "  |-  " ++ t ++ " ~ " ++  u)
  (wellFormed kEnv t' && wellFormed kEnv u' && equivalent kEnv t' u' `shouldBe` True)
  where
    [t', u'] = renameTypes [read t, read u]
    kEnv = readKenv k

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ snd $ runState (synthetise kEnv t) (initialState "Kind synthesis for testing type equivalence")

spec :: Spec
spec = do
  tests <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

