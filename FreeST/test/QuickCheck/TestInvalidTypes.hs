module TestInvalidTypes
( prop_not_bisimilar
) where

import           ArbitraryTypes
import           Equivalence.Equivalence
import           Validation.Kinding
import           Syntax.Type
import           Syntax.Kind
import           Syntax.Base hiding (pos)
import           Utils.FreestState
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

main = quickCheckWith stdArgs {maxSuccess = 1000} prop_not_bisimilar

bisim :: Type -> Type -> Bool
bisim = Equivalence.Equivalence.bisimilar Map.empty

pos = defaultPos

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")

prop_not_bisimilar :: NonBisimPair -> Property
prop_not_bisimilar (NonBisimPair t u) = kinded t && kinded u ==> not (t `bisim` u)
