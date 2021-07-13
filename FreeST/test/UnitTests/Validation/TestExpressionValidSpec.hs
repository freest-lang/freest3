module Validation.TestExpressionValidSpec
  ( spec
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        
import           Elaboration.Duality           as Dual       
import           SpecUtils
import           Syntax.Expression
import           Util.FreestState
import           Util.PreludeLoader            ( prelude )
import           Validation.Typing              ( checkAgainst )
import Validation.Rename
import Debug.Trace

spec :: Spec
spec = describe "Valid expressions" $ do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestExpressionValid.txt"
  mapM_ matchValidExpressionSpec (chunksOf 2 t)


matchValidExpressionSpec :: [String] -> Spec
matchValidExpressionSpec [e, t] =
  it (e ++ " : " ++ t) $
    isExpr (read e) (read t) `shouldBe` Left True

isExpr :: Exp -> Type -> TestExpectation
isExpr e t = testValidExpectation True (errors s) -- null (errors s)
 where
  s    = execState test initialState { varEnv = prelude }
  test = do
    t' <- rename Map.empty =<< Dual.resolve t
    e' <- rename Map.empty =<< Dual.resolve e
    checkAgainst Map.empty e' t'
