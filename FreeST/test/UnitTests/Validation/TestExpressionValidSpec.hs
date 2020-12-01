module Validation.TestExpressionValidSpec (spec) where

import           SpecHelper
import           Syntax.Expression
import           Control.Monad.State
import           Utils.FreestState
import           Validation.Typing(checkAgainst)
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Valid expressions" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
    mapM_ matchValidExpressionSpec (chunksOf 2 t)


matchValidExpressionSpec :: [String] -> Spec
matchValidExpressionSpec [e, t] =
  it (e ++ " : " ++ t) $ isExpr (read e) (read t) `shouldBe` True

isExpr :: Exp -> Type -> Bool
isExpr e t = null (errors s)
  where s = execState (checkAgainst Map.empty e t) (initialState "Check Against Expression")
