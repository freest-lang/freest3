import qualified Bisimulation0 as B0
-- B1
import qualified Bisimulation2 as B2
import qualified Bisimulation3 as B3
import qualified Bisimulation4 as B4
 -- B12
 -- B13
 -- B14
import qualified Bisimulation23 as B23
import qualified Bisimulation24 as B24
import qualified Bisimulation34 as B34
-- B123
-- B124
-- B134
import qualified Bisimulation234 as B234
import qualified Bisimulation1234 as B1234

import qualified TypeToGrammar as TG
import qualified TypeToGrammar1 as TG1


import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import ArbitraryTypes
import qualified Data.Map.Strict as Map

-- Freest
import           Validation.Kinding
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Syntax.Duality
import           Utils.FreestState
import           Control.Monad.State
import           Debug.Trace




-- 1 + binomial(4, 1) + binomial(4, 2) + binomial(4, 3) + 1

-- bisimCombs :: [(String, TypeEnv -> Type -> Type -> Bool)]
bisimCombs tenv t u = Map.fromList
  [ ("B0", B0.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B1", B0.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B2", B2.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B3", B3.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B4", B4.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B12", B2.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B13", B3.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B14", B4.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B23", B23.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B24", B23.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B34", B34.bisimilar $ TG.convertToGrammar tenv [t, u])  
  , ("B123", B23.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B124", B24.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B134", B34.bisimilar $ TG1.convertToGrammar tenv [t, u])
  , ("B234", B234.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B1234", B234.bisimilar $ TG1.convertToGrammar tenv [t, u])
  ]

-- bisimilar :: TypeEnv -> Type -> Type -> Bool
-- bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]      
                     

-- bisimilar :: TypeEnv -> Type -> Type -> Bool
-- bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]

main :: IO ()
main =
  defaultMain [
    bench "Bisimilar0" $ whnfIO ( 
      quickCheckWith stdArgs {maxSuccess = 3557, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar)

    , bench "Bisimilar1" $ whnfIO (
      quickCheckWith stdArgs {maxSuccess = 3557, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar1)
      
    , bench "Bisimilar1234" $ whnfIO (
      quickCheckWith stdArgs {maxSuccess = 3557, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar1234)
  ]
       
       
-- main = defaultMain [
--          bench "fib 10" $ \n -> fib (10+n-n)
--        , bench "fib 30" $ \n -> fib (30+n-n)
--        , bench "fib 35" $ \n -> fib (35+n-n)
--        ]

--- QuickCheck

pos :: Pos
pos = defaultPos

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")

-- Bisimilar types are bisimilar
prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t && kinded u ==>
  B0.bisimilar $ TG.convertToGrammar Map.empty [t, u]

prop_bisimilar1 :: BisimPair -> Property
prop_bisimilar1 (BisimPair t u) = kinded t && kinded u ==>
 bisimCombs Map.empty t u Map.! "B1"

-- Bisimilar types are bisimilar
prop_bisimilar1234 :: BisimPair -> Property
prop_bisimilar1234 (BisimPair t u) = kinded t && kinded u ==>
  bisimCombs Map.empty t u Map.! "B1234"

{-
benchmarking Bisimilar0
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
time                 211.0 ms   (206.3 ms .. 215.0 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 213.9 ms   (212.4 ms .. 215.9 ms)
std dev              2.233 ms   (1.416 ms .. 2.984 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Bisimilar1
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
time                 53.56 s    (53.27 s .. 54.17 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 53.89 s    (53.69 s .. 54.19 s)
std dev              299.6 ms   (66.37 ms .. 395.4 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Bisimilar1234
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
+++ OK, passed 3557 tests; 1298 discarded.
time                 53.66 s    (53.63 s .. 53.67 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 53.65 s    (53.63 s .. 53.66 s)
std dev              11.43 ms   (39.96 μs .. 15.15 ms)

-}
