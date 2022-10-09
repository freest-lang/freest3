{-# LANGUAGE BlockArguments #-}

module Subtyping.TestSubValidSpec (spec) where

import qualified Syntax.Type as T
import           Validation.Kinding               ( synthetise )
import           Validation.Rename
import           Bisimulation.Bisimulation        ((<~))
import qualified Data.Map.Strict           as Map
import           Util.FreestState                 ( initialState
                                                  , errors, FreestState
                                                  )
import           Control.Monad.State              ( execState )
import           SpecUtils

matchValidSpec :: [String] -> Spec
matchValidSpec [st, su] =
  it (show t ++ " <~ " ++  show u) 
    ( {-# SCC "SUB_TEST_CALL" #-}
    {-           wellFormed t
    &&           wellFormed u
    &&-}         (t <~ u) 
    `shouldBe` True
    )
    where
      [t, u] = renameTypes [read st, read su]

wellFormed :: Type -> Bool
wellFormed t = null $ errors $ execState (synthetise Map.empty t) initialState

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Subtyping/TestSubValid.txt"
  describe "Valid Subtyping Test" $ mapM_ matchValidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
