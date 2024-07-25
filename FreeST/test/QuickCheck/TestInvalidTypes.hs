module TestInvalidTypes
( prop_not_bisimilar
) where

import           ArbitraryTypes
import           Equivalence.TypeEquivalence   ( bisimilar )
import           Kinding.Kinding
import           Typing.Phase
import           Syntax.Type
import           Syntax.Kind              as K
import           Syntax.Base
import           Util.State

import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import           Test.QuickCheck


main = quickCheckWith stdArgs {maxSuccess = 1000} prop_not_bisimilar

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kEnv t) (initialTyp defaultOpts) --  "Kind synthesis")

prop_not_bisimilar :: NonBisimPair -> Property
prop_not_bisimilar (NonBisimPair t u) = kinded t && kinded u ==> not (t `bisimilar` u)
