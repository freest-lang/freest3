module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import qualified Syntax.Kind as K
import           Elaboration.ResolveDuality
import qualified Elaboration.Phase as EP
import qualified Typing.Phase as VP
import           Typing.Rename ( renameTypes )
import           Kinding.Kinding ( synthetise )
import           Equivalence.TypeEquivalence ( equivalent, bisimilar )
import           SpecUtils
import           Util.State ( initial, errors, defaultOpts, Errors )

import Control.Monad.State ( execState, evalState )

matchValidSpec :: [String] -> Spec
matchValidSpec [k,t,u] = it
  (k ++ "  |-  " ++ t ++ " ~ " ++ u)
  (testValidExpectation (bisimilar t' u') (concatMap (kinded kEnv) ts) `shouldBe` Left True)
  where
    kEnv = readKenv k
    ts@[t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]
matchValidSpec xs = it ("Something fundamentally wrong happened: "++show xs) (False `shouldBe` True)

resolveDuals :: Type -> Type
resolveDuals t = evalState (resolve t) (initial EP.extraElab)

kinded :: K.KindEnv -> Type -> Errors
kinded kEnv t =
  errors $ execState (synthetise kEnv t) (VP.initialTyp defaultOpts)

spec :: Spec
spec = do
  tests <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

