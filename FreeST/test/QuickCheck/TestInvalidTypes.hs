module TestInvalidTypes
( prop_not_bisimilar
) where

import           ArbitraryTypes
import           Bisimulation.Bisimulation       ( bisimilar )
import           Validation.Kinding
import           Syntax.Type
import           Syntax.Kind              as K
import           Syntax.Base
import           Util.FreestState
import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Test.QuickCheck

main = quickCheckWith stdArgs {maxSuccess = 1000} prop_not_bisimilar

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar defaultPos) ids) (repeat (K.sl defaultPos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise Set.empty kindEnv t) (initialState "Kind synthesis")

prop_not_bisimilar :: NonBisimPair -> Property
prop_not_bisimilar (NonBisimPair t u) = kinded t && kinded u ==> not (t `bisimilar` u)
