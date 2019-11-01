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
import           Parse.Parser
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

test :: BisimPair -> BisimFunction -> Bool
test (BisimPair t u) bisim = bisim Map.empty t u

clockSomething :: a -> IO (String, a)
clockSomething something = do
  start <- getTime Monotonic
  r = (evaluate $ something)
  end <- getTime Monotonic
  return $ (formatToString (timeSpecs) start end, r)

parseTestArgs :: [String] -> (Int, Int, Int, Boolean)
parseTestArgs [] = (0, 0, 0, True)
parseTestArgs (seed:version:depth:pos:[]) = (read seed, read version, read depth, read:pos)


runTestVersion :: BisimPair -> BisimFunction -> String -> IO String
runTestVersion p f name = do
    -- putStrLn $ show p
    time <- clockSomething (test p f)
    return time
    
nodesOf :: BisimPair -> (String, String)
nodesOf (BisimPair t1 t2) = (show $ nodes t1, show $nodes t2)

runEach :: (BisimPair, Int) -> (String, BisimFunction) -> Int -> Bool -> IO ()
runEach (pair, d) (name, f) seed pos = do
    (v,r) <- timeout (30 * 60 * seconds_in_micro) $ runTestVersion pair f name
    let (n1, n2) = nodesOf pair
    let base = name ++ ";"  ++ n1 ++ ";" ++ n2 ++ ";" ++ (show d) ++ ";" ++ (show seed) ++ ";" ++ (show pos) ++ ";" ++ (show r) ++ ";"
    case v of
        Nothing -> putStrLn $ base ++ "timeout"
        (Just time) -> putStrLn $ base ++ time

mkPairPositive :: Int -> Int -> IO BisimPair
mkPairPositive seed depth = do
    let g = mkStdGen seed
    let (v, ng) = random g :: (Int, StdGen)
    
    let generator = mkQCGen $ v in
    let pair = unGen (arbitrary :: Gen BisimPair) generator depth in
    
    if kinded t1 && kinded t2 then do
      return pair
    else do
      let (v, _) = random ng :: (Int, StdGen)
      mkPairPositive v depth
    
mkPairNegative :: Int -> Int -> IO BisimPair
mkPairNegative seed depth = do
    -- Disable because each run only generates one pair
    let g = mkStdGen seed
    let (v, ng) = random g :: (Int, StdGen)
    let generator1 = mkQCGen $ seed
    let generator2 = mkQCGen $ v
    let t1 = unGen (arbitrary :: Gen Type) generator1 depth
    let t2 = unGen (arbitrary :: Gen Type) generator2 depth
    let pair = BisimPair t1 t2
    if kinded t1 && kinded t2 && not (test pair B0.bisimilar) then do
      return pair
    else do
      let (v, _) = random ng :: (Int, StdGen)
      mkPairNegative v depth

main :: IO ()
main = do
    arguments <- getArgs
    let (seed, version, depth, pos) = parseTestArgs arguments
    let pairF = if pos then mkPairPositive else mkPairNegative in
    runEach (pairF seed depth, depth) (bisims !! version) seed pos
