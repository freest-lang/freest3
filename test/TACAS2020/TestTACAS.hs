import           System.Environment
import           System.IO
import           System.Clock
import           System.Random
import           System.Timeout
import           Control.Monad
import           Control.Exception
import           Control.DeepSeq
import           Formatting
import           Formatting.Clock
import           QuickCheck.ArbitraryTypes
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import           Syntax.Schemes
import           Syntax.Types
import           QuickCheck.TestValidTypes
import qualified Data.Map as Map
import qualified TACAS2020.Bisimulation0 as B0
import qualified TACAS2020.Bisimulation1 as B1
import qualified TACAS2020.Bisimulation2 as B2
import qualified TACAS2020.Bisimulation3 as B3
import qualified TACAS2020.Bisimulation4 as B4
import qualified TACAS2020.Bisimulation12 as B12
import qualified TACAS2020.Bisimulation123 as B123
import qualified TACAS2020.Bisimulation1234 as B1234

seconds_in_micro = 1000000

bisims = [ ("B0", B0.bisimilar)
         , ("B1", B1.bisimilar)
         , ("B2", B2.bisimilar)
         , ("B3", B3.bisimilar)
         , ("B4", B4.bisimilar)
         , ("B12", B12.bisimilar)
         , ("B123", B123.bisimilar)
         , ("B1234", B1234.bisimilar)
         ]

type BisimFunction = (TypeEnv -> Type -> Type -> Bool)

test :: BisimPair -> BisimFunction -> Property
test (BisimPair t u) bisim = kinded t && kinded u ==> bisim Map.empty t u

clockSomething :: a -> IO String
clockSomething something = do
  start <- getTime Monotonic
  void (evaluate $ something)
  end <- getTime Monotonic
  return $ formatToString (timeSpecs) start end

parseTestArgs :: [String] -> Int
parseTestArgs [] = 0
parseTestArgs (x:_) = read x


runTestVersion :: BisimPair -> BisimFunction -> String -> IO String
runTestVersion p f name = do
    time <- clockSomething (test p f)
    return time

runTest :: Int -> Int -> IO ()
runTest size seed = do
    let generator = mkQCGen $ seed
    let pair = unGen (arbitrary :: Gen BisimPair) generator size
    mapM_ (runEach pair) bisims

runEach :: BisimPair -> (String, BisimFunction) -> IO ()
runEach pair (name, f) = do
    v <- timeout (60 * seconds_in_micro) $ runTestVersion pair f name
    let base = name ++ ";" -- ++ (show $ nodes $ typeOf pair) ++ ";"
    case v of
        Nothing -> putStrLn $ base ++ "timeout"
        (Just time) -> putStrLn $ base ++ time

runTests :: StdGen -> Int -> IO ()
runTests _ 0 = return ()
runTests g n = do
    let (v, g2) = random g :: (Int, StdGen)
    runTest v (n `mod` 5)
    runTests g2 (n-1)


main :: IO ()
main = do
    arguments <- getArgs
    let seed = parseTestArgs arguments
    runTests (mkStdGen seed) 100 
