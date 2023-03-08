module Equivalence.TestEquivalenceInvalidSpec
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

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [k, t, u]  |
  wellFormed kEnv t' &&
  wellFormed kEnv u' = it
    (k ++ "  |-  " ++ t ++ " ~ " ++ u)
    (bisimilar t' u' `shouldBe` False)
  where
    [t', u'] = renameTypes [read t, read u]
    kEnv     = readKenv k

wellFormed :: K.KindEnv -> Type -> Bool
wellFormed kEnv t = null $ errors $ execState (synthetise kEnv t) initialState

spec :: Spec
spec = do
  t <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ mapM_ matchInvalidSpec (chunksOf 3 t)

main :: IO ()
main = hspec spec
