module Validation.TestTypesInvalidSpec
  ( spec
  )
where

import           Control.Monad.State ( execState )
import qualified Data.Map.Strict as Map ( empty )
import           Elaboration.ResolveDuality as Dual
import           SpecUtils
import qualified Syntax.Type as T
import           Util.State ( initialS, errors )
import           Validation.Kinding ( synthetise )
import           Validation.Rename ( renameType )
import  Elaboration.Phase
import           Text.Read

spec :: Spec
spec = describe "Invalid types tests" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesInvalid.txt"
  mapM_ matchInvalidKindingSpec t

matchInvalidKindingSpec :: String -> Spec
matchInvalidKindingSpec t = it t $ isWellFormed t `shouldBe` False

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- If the value is Left a, apply the first function to a;
-- if it is Right b, apply the second function to b.

isWellFormed :: String -> Bool
isWellFormed str =  either (const False) synthetiseK (parseType "TestTypesInvalidSpec" str)
 where
  synthetiseK t = null $ errors $ execState
    (synthetise Map.empty . renameType =<< Dual.resolve t) initialElab

main :: IO ()
main = hspec spec
