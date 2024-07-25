module Equivalence.TestEquivalenceInvalidSpec
  ( spec
  )
where

import Equivalence.TypeEquivalence ( equivalent )
import Control.Monad.State ( execState, evalState )
import SpecUtils
import Syntax.Kind as K
import Util.State ( initial, errors, initialS, Errors )
import Kinding.Kinding ( synthetise )
import Typing.Rename ( renameTypes )
import Elaboration.ResolveDuality ( resolve )
import Elaboration.Phase ( extraElab )

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [k,t,u] = it
  (k ++ "  |-  " ++ t ++ " /~ " ++ u)
  (testValidExpectation (equivalent t' u') (concatMap (wellFormed kEnv) ts) `shouldBe` Left False)
  where
    kEnv = readKenv k
    ts@[t', u'] = renameTypes [resolveDuals $ read t, resolveDuals $ read u]
matchInvalidSpec xs = it ("Something fundamentally wrong happened: "++show xs) (False `shouldBe` True)

resolveDuals :: Type -> Type
resolveDuals t = evalState (resolve t) (initial extraElab)

wellFormed :: K.KindEnv -> Type -> Errors
wellFormed kEnv t = errors $ execState (synthetise kEnv t) initialS

spec :: Spec
spec = do
  t <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ mapM_ matchInvalidSpec (chunksOf 3 t)

main :: IO ()
main = hspec spec
