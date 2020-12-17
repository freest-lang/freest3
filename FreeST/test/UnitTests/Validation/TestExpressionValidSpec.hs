module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        ( subsExp
                                                , subsType
                                                )
import           SpecHelper
import           Syntax.Expression
import           Utils.FreestState
import           Utils.PreludeLoader            ( prelude )
import           Validation.Typing              ( checkAgainst )

spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  mapM_ matchValidExpressionSpec (chunksOf 2 t)


matchValidExpressionSpec :: [String] -> Spec
matchValidExpressionSpec [e, t] =
  it (e ++ " : " ++ t) $ isExpr (read e) (read t) `shouldBe` True

isExpr :: Exp -> Type -> Bool
isExpr e t = null (errors s)
 where
  s    = execState test is
  is   = (initialState "Check Against Expression") { varEnv = prelude }
  test = join $ liftM2 (checkAgainst Map.empty)
                       (subsExp Map.empty e)
                       (subsType Map.empty Nothing t)

