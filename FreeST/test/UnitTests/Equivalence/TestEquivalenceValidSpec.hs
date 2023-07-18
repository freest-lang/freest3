module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import Bisimulation.Bisimulation ( bisimilar )
import Control.Monad.State ( execState, evalState )
import Elaboration.ResolveDuality
import SpecUtils
import Syntax.Kind as K
import Util.State ( initial, errors, defaultOpts )
import Validation.Kinding ( synthetise )
import Validation.Rename
import qualified Elaboration.Phase as EP
import qualified Validation.Phase as VP


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
resolveDuals t = evalState (resolve t) (initial EP.extraElab)

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ execState (synthetise kEnv t) (VP.initialTyp defaultOpts)

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

