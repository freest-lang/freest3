module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Syntax.Kind                   as K
import           Bisimulation.Bisimulation      ( bisimilar )
import           Validation.Rename
import           Validation.Kinding             ( synthetise )
import           SpecUtils
import           Util.FreestState               ( initialState
                                                , errors
                                                )
import           Control.Monad.State            ( execState )

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] |
  wellFormed kEnv t' &&
  wellFormed kEnv u' = it
    (k ++ "  |-  " ++ t ++ " ~ " ++ u)
    (bisimilar t' u' `shouldBe` True)
  where
    [t', u'] = renameTypes [read t, read u]
    kEnv     = readKenv k

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ execState (synthetise kEnv t) initialState

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

