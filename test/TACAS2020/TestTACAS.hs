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

clockSomething :: a -> IO String
clockSomething something = do
  start <- getTime Monotonic
  void (evaluate $ something)
  end <- getTime Monotonic
  return $ formatToString (timeSpecs) start end

parseTestArgs :: [String] -> (Int, Int, Int)
parseTestArgs [] = (0, 0, 0)
parseTestArgs (seed:version:depth:[]) = (read seed, read version, read depth)


runTestVersion :: BisimPair -> BisimFunction -> String -> IO String
runTestVersion p f name = do
    -- putStrLn $ show p
    time <- clockSomething (test p f)
    return time
    
nodesOf :: BisimPair -> (String, String)
nodesOf (BisimPair t1 t2) = (show $ nodes t1, show $nodes t2)

runEach :: (BisimPair, Int) -> (String, BisimFunction) -> Int -> IO ()
runEach (pair, d) (name, f) seed = do
    v <- timeout (60 * seconds_in_micro) $ runTestVersion pair f name
    let (n1, n2) = nodesOf pair
    let base = name ++ ";"  ++ n1 ++ ";" ++ n2 ++ ";" ++ (show d) ++ ";" ++ (show seed) ++ ";"
    case v of
        Nothing -> putStrLn $ base ++ "timeout"
        (Just time) -> putStrLn $ base ++ time

mkPair :: Int -> Int -> BisimPair
mkPair seed depth =
    -- Disable because each run only generates one pair
    -- let g = mkStdGen seed
    -- let (v, _) = random g :: (Int, StdGen)
    let generator = mkQCGen $ seed in
    let pair = unGen (arbitrary :: Gen BisimPair) generator depth in
    pair
    
    --let t1 = read "((rec w:SL. &{B: ((+{A: &{B: x, C: Skip}};(+{A: ?Bool, B: w, C: (?Int;(&{A: ((?();(x;w));((Skip;+{A: w, C: Skip});!Int)), C: !Char};w))};Skip));+{A: (!Char;(((?Int;(&{A: ((?();(x;w));((Skip;+{A: w, C: Skip});!Int)), C: !Char};w));?Char);(?Int;(&{A: ((?();(x;w));((Skip;+{A: w, C: Skip});!Int)), C: !Char};w))))}), C: Skip});(x;((&{B: !Bool, C: Skip};((rec δ:SU. ?());y));(?Int;(+{B: x, C: ?Int};(!Int;Skip))))))" in
    --let t2 = read "((rec z:SL. (Skip;&{B: (+{A: (&{B: x, C: Skip};+{A: (?Bool;Skip), B: (z;Skip), C: (((?Int;&{A: ((((?();x);z);+{A: z, C: Skip});!Int), C: !Char});z);Skip)})};(+{A: !Char};(((?Int;&{A: ((((?();x);z);+{A: z, C: Skip});!Int), C: !Char});z);(?Char;((?Int;&{A: ((((?();x);z);+{A: z, C: Skip});!Int), C: !Char});z))))), C: Skip}));((x;&{B: (!Bool;(?();y)), C: (Skip;(?();y))});((?Int;+{B: x, C: ?Int});!Int)))" in
    --BisimPair t1 t2
    

main :: IO ()
main = do
    arguments <- getArgs
    let (seed, version, depth) = parseTestArgs arguments
    runEach (mkPair seed depth, depth) (bisims !! version) seed
