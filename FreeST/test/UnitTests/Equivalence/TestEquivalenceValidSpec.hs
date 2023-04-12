module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Syntax.Kind                   as K
import           Bisimulation.Bisimulation      ( bisimilar )
import           Validation.Rename
import           Elaboration.ResolveDuality
import           Validation.Kinding             ( synthetise )
import           SpecUtils
import           Util.FreestState               ( initialState
                                                , errors
                                                )
import           Control.Monad.State            ( execState, evalState )

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] |
  wellFormed kEnv t' &&
  wellFormed kEnv u' = it
    (k ++ "  |-  " ++ t ++ " ~ " ++ u)
    (bisimilar t' u' `shouldBe` True)
  where
    kEnv = readKenv k
    [t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]

resolveDuals :: Type -> Type
resolveDuals t = evalState (resolve t) initialState

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ execState (synthetise kEnv t) initialState

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

