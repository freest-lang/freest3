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
import Test.QuickCheck.Random
import Test.QuickCheck.Gen
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


import           System.Timeout
import           System.Random
import           System.Environment
import           System.IO
import           System.Clock
import           System.Random
import           System.Timeout
import           Formatting
import           Formatting.Clock

import           Control.Exception

-- 1m
timeoutInMicro = 60000000
-- depth = 1000000
-- seed = 1095646480

type BisimFunction = (TypeEnv -> Type -> Type -> Bool)


clockSomething :: a -> IO (String, a)
clockSomething something = do
  start <- getTime Monotonic
  r <- (evaluate $ something)
  end <- getTime Monotonic
  return $ (formatToString (timeSpecs) start end, r)


mkPairPositive :: Int -> Int -> IO BisimPair
mkPairPositive seed depth = do
    let g = mkStdGen seed
    let (v, ng) = random g :: (Int, StdGen)
    
    let generator = mkQCGen $ v
    let (BisimPair t1 t2) = unGen (arbitrary :: Gen BisimPair) generator depth
    
    if kinded t1 && kinded t2 then do
      return (BisimPair t1 t2)
    else do
      let (v, _) = random ng :: (Int, StdGen)
      mkPairPositive v depth

nodesOf :: BisimPair -> (String, String)
nodesOf (BisimPair t1 t2) = (show $ nodes t1, show $nodes t2)

-- 1 + binomial(4, 1) + binomial(4, 2) + binomial(4, 3) + 1

-- bisimCombs :: [(String, TypeEnv -> Type -> Type -> Bool)]
bisimCombs = -- Map.fromList
  [ ("B0", B0.bisimilar, TG.convertToGrammar)
  -- , ("B1", B0.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B2", B2.bisimilar $ TG.convertToGrammar tenv [t, u])
  -- , ("B3", B3.bisimilar $ TG.convertToGrammar tenv [t, u])
  -- , ("B4", B4.bisimilar $ TG.convertToGrammar tenv [t, u])
  -- , ("B12", B2.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B13", B3.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B14", B4.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B23", B23.bisimilar $ TG.convertToGrammar tenv [t, u])
  -- , ("B24", B23.bisimilar $ TG.convertToGrammar tenv [t, u])
  -- , ("B34", B34.bisimilar $ TG.convertToGrammar tenv [t, u])  
  -- , ("B123", B23.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B124", B24.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B134", B34.bisimilar $ TG1.convertToGrammar tenv [t, u])
  -- , ("B234", B234.bisimilar $ TG.convertToGrammar tenv [t, u])
  , ("B1234", B234.bisimilar, TG1.convertToGrammar)
  ]

-- bisimilar :: TypeEnv -> Type -> Type -> Bool
-- bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]      
                     


-- bisimilar :: TypeEnv -> Type -> Type -> Bool
-- bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]

 -- secs <$> time_ someIOFunction >>= print
main :: IO ()
main = do
  args <- getArgs
  let (seed, version, depth, pos) = parseTestArgs args  
  pair <- mkPairPositive seed depth
  runEach pair depth (bisimCombs !! version) seed pos

parseTestArgs :: [String] -> (Int, Int, Int, Bool)
parseTestArgs [] = (0, 0, 0, True)
parseTestArgs (seed:version:depth:pos:[]) = (read seed, read version, read depth, read pos)
  
runEach pair depth (name, f, g) seed pos = do
  v <-  timeout timeoutInMicro $ runTestVersion pair f g
  let (n1, n2) = nodesOf pair
  let base = name ++ ";"  ++ n1 ++ ";" ++ n2 ++ ";" ++ (show depth)
                  ++ ";" ++ (show seed) ++ ";" ++ (show pos) ++ ";"
  case v of
        Nothing -> putStrLn $ base ++ "timeout"
        (Just (time, r)) -> putStrLn $ base ++ (show r) ++ ";" ++ time


-- runTestVersion :: BisimPair -> String -> IO (String, Bool)
runTestVersion (BisimPair t u) f g = do
    -- putStrLn $ show p
    res <- clockSomething (f $ g Map.empty [t, u])
    return res

-- test :: BisimPair -> BisimFunction -> Bool
-- test (BisimPair t u) bisim = bisim Map.empty t u


-- QuickCheck

pos :: Pos
pos = defaultPos

nodes :: Type -> Int
nodes (Semi _ t u)   = 1 + nodes t + nodes u
nodes (Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (Rec _ _ t)    = 1 + nodes t
-- Skip, Message, TypeVar
nodes _              = 1

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")

-- prop_bisimilar :: String -> BisimPair -> Property
-- prop_bisimilar str (BisimPair t u) = kinded t && kinded u ==>
--  bisimCombs Map.empty t u Map.! str
