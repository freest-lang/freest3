module Validation.TestTypesInvalidSpec
  ( spec
  )
where

import           Control.Monad.State                ( execState )
import qualified Data.Map.Strict            as Map  ( empty )
import           Elaboration.ResolveDuality as Dual
import           SpecUtils
import           Util.FreestState                   ( initialState, errors )
import           Validation.Kinding                 ( synthetise )
import           Validation.Rename                  ( renameType )
import qualified Syntax.Type as T

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
isWellFormed str =  either (const False) synthetiseK (parseTypeEither str)
 where
  synthetiseK t = null $ errors $ execState
    (synthetise Map.empty . renameType =<< Dual.resolve t) initialState

main :: IO ()
main = hspec spec
