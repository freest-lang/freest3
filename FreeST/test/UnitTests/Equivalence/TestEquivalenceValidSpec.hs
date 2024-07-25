module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Syntax.Kind as K
import           Elaboration.ResolveDuality
import qualified Elaboration.Phase as EP
import qualified Typing.Phase as VP
import           Typing.Rename ( renameTypes )
import           Kinding.Kinding ( synthetise )
import           Equivalence.TypeEquivalence ( equivalent, bisimilar )
import           SpecUtils
import           Util.State ( initial, errors, defaultOpts )

import Control.Monad.State ( execState, evalState )

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] |
  kinded kEnv t' &&
  kinded kEnv u' = it
    (k ++ "  |-  " ++ t ++ " ~ " ++ u)
    (bisimilar t' u' `shouldBe` True)
  where
    kEnv = readKenv k
    [t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]
matchValidSpec [k,t,u] =
  it ("malformed: "++show (filter (not . kinded kEnv) [t',u'])) (False `shouldBe` True)
  where
    kEnv = readKenv k
    [t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]
  -- Why not accept "Non-exhaustive patterns"?
matchValidSpec xs = it ("Something fundamentally wrong happened: "++show xs) (False `shouldBe` True)

resolveDuals :: Type -> Type
resolveDuals t = evalState (resolve t) (initial EP.extraElab)

kinded :: K.KindEnv -> Type -> Bool
kinded kEnv t =
  null $ errors $ execState (synthetise kEnv t) (VP.initialTyp defaultOpts)

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

