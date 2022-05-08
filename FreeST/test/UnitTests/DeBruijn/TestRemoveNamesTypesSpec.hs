module DeBruijn.TestRemoveNamesTypesSpec
  ( spec
  )
where

import           Control.Monad.State            ( runState )
import qualified Data.Map.Strict               as Map
                                                ( empty )
import           Elaboration.ResolveDuality    as Dual
import           SpecUtils
import           Syntax.Kind                    ( Kind )
import           Util.FreestState               ( initialState
                                                , errors, FreestS
                                                )
import           Validation.Kinding             ( synthetise )
import           Validation.Rename              ( renameType )
import           Validation.Subkind             ( (<:) )
import Util.DeBruijn (Nameless(removeNames), NamelessType, aeq)

-- is the nameless term resulting from removeNames alpha-equivalent to the given nameless term?

spec :: Spec
spec = describe "Valid type tests" $ do
  t <- runIO $ readFromFile "test/UnitTests/DeBruijn/TestRemoveNamesTypes.txt"
  mapM_ removeNamesEqSpec (chunksOf 2 t)

removeNamesEqSpec :: [String] -> Spec
removeNamesEqSpec [snamed, snameless] =
    it snamed $ removeNamesEq namesRemoved nameless s `shouldBe` Left True
  where named = read snamed
        nameless = read snameless
        (namesRemoved, s) = runState (removeNames [] 0 named) initialState

removeNamesEq :: NamelessType -> NamelessType -> FreestS -> TestExpectation
removeNamesEq namesRemoved nameless s =
  testValidExpectation (namesRemoved `aeq` nameless) (errors s) -- null (errors s) && k' <: k

main :: IO ()
main = hspec spec
