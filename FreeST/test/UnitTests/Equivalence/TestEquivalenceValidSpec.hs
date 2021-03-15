module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Syntax.Kind                   as K
import           Equivalence.Equivalence
import           Validation.Rename
import           Validation.Kinding             ( synthetise )
import           SpecUtils
import           Util.FreestState              ( initialState
                                                , errors
                                                )
import           Control.Monad.State            ( execState )

import qualified Data.Set as Set

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] = it
  (k ++ "  |-  " ++ t ++ " ~ " ++ u)
  (          wellFormed kEnv t'
  &&         wellFormed kEnv u'
  &&         equivalent kEnv t' u'
  `shouldBe` True
  )
 where
  [t', u'] = renameTypes [read t, read u]
  kEnv     = readKenv k

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ execState
  (synthetise Set.empty kEnv t)
  (initialState "Kind synthesis for testing type equivalence")

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

