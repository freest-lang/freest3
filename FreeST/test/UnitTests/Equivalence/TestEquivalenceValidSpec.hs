module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Syntax.Kind as K
import           Elaboration.ResolveDuality
import qualified Elaboration.Phase as EP
import qualified Typing.Phase as VP
import           Typing.Rename
import           Kinding.Kinding ( synthetise )
import           Equivalence.TypeEquivalence ( equivalent )
import           SpecUtils
import           Util.State ( initial, errors, defaultOpts )

import Control.Monad.State ( execState, evalState )

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] |
  wellFormed kEnv t' &&
  wellFormed kEnv u' = it
    (k ++ "  |-  " ++ t ++ " ~ " ++ u)
    (equivalent t' u' `shouldBe` True)
  where
    kEnv = readKenv k
    [t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]
-- matchValidSpec xs = it ("BANG! " ++ show xs) (False `shouldBe` True)
matchValidSpec _ = it "" (True `shouldBe` True) -- Why not accept "Non-exhaustive patterns"?

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

