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

import Parse.Parser

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
import           Validation.Rename
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

type BisimFunction = (TypeEnv -> Type -> Type -> Bool)
-- 1m
timeoutInMicro = 2 * 60 * 1000000

-- Bisim functions combinations
-- bisimCombs :: [(String, TypeEnv -> Type -> Type -> Bool)]
bisimCombs = -- Map.fromList
  [ ("B0", B0.bisimilar, TG.convertToGrammar)
  , ("B1", B0.bisimilar, TG1.convertToGrammar)
  , ("B2", B2.bisimilar, TG.convertToGrammar)
  , ("B3", B3.bisimilar, TG.convertToGrammar)
  , ("B4", B4.bisimilar, TG.convertToGrammar)
  , ("B12", B2.bisimilar, TG1.convertToGrammar)
  , ("B13", B3.bisimilar, TG1.convertToGrammar)
  , ("B14", B4.bisimilar, TG1.convertToGrammar)
  , ("B23", B23.bisimilar, TG.convertToGrammar)
  , ("B24", B23.bisimilar, TG.convertToGrammar)
  , ("B34", B34.bisimilar, TG.convertToGrammar)  
  , ("B123", B23.bisimilar, TG1.convertToGrammar)
  , ("B124", B24.bisimilar, TG1.convertToGrammar)
  , ("B134", B34.bisimilar, TG1.convertToGrammar)
  , ("B234", B234.bisimilar, TG.convertToGrammar)
  , ("B1234", B1234.bisimilar, TG1.convertToGrammar)
  ]

-- Get exectime of a computation
clockSomething :: a -> IO (String, a)
clockSomething something = do
  start <- getTime Monotonic
  r <- (evaluate $ something)
  end <- getTime Monotonic
  return $ (formatToString (timeSpecs) start end, r)


parseTestArgs :: [String] -> (BisimPair, Int, Bool)
parseTestArgs [] = (BisimPair defaultType defaultType, 0, True)
parseTestArgs (t:u:version:positive:_) =
  (BisimPair t' u', read version, read positive)
  where
    [t',u'] = renameTypes [read t, read u]



defaultType :: Type
defaultType = Skip defaultPos
  
runEach pair (name, f, g) pos = do
  v <-  timeout timeoutInMicro $ runTestVersion pair f g

  let (n1, n2) = nodesOf pair
  let base = name ++ ";"  ++ n1 ++ ";" ++ n2 ++ ";" ++ (show pos) ++ ";"
  let (BisimPair t1 t2) = pair
  case v of
        Nothing -> putStrLn $ base ++ "timeout"
        (Just (time, r)) -> putStrLn $ base ++ (show r) ++ ";" ++ time
        -- (Just (time, True)) -> putStrLn $ base ++ (show True) ++ ";" ++ time
        --    ++ "\nTypes:\n" ++ show t1 ++ "\n" ++ show t2 ++ "\n\n"
        -- (Just (time, False)) -> putStrLn $ base ++ (show False) ++ ";" ++ time


-- runTestVersion :: BisimPair -> String -> IO (String, Bool)
runTestVersion (BisimPair t u) f g = do
    res <- clockSomething (f $ g Map.empty [t, u])
    return res



-- QuickCheck

pos :: Pos
pos = defaultPos

nodes :: Type -> Int
nodes (Semi _ t u)   = 1 + nodes t + nodes u
nodes (Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (Rec _ _ t)    = 1 + nodes t
-- Skip, Message, TypeVar
nodes _              = 1

nodesOf :: BisimPair -> (String, String)
nodesOf (BisimPair t1 t2) = (show $ nodes t1, show $nodes t2)

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")


parseGenArgs :: [String] -> (Int,Int,Int,Bool)
parseGenArgs [] = (0,0,0,True)  
parseGenArgs (sizeLower:sizeUpper:depth:pos:_) = (read sizeLower, read sizeUpper, read depth, read pos)   

-- Gen positive types
mkPairPositive ::  Int -> Int -> Int -> IO BisimPair
mkPairPositive sizeLower sizeUpper depth = do
    (BisimPair t1 t2) <- generate (arbitrary :: Gen BisimPair) -- generator depth

    let n1 = nodes t1
    let n2 = nodes t2
    
    if (n1 > sizeLower && n1 <= sizeUpper) || (n2 > sizeLower && n2 <= sizeUpper) then
      if kinded t1 && kinded t2 then
        return (BisimPair t1 t2)
      else mkPairPositive sizeLower sizeUpper depth
    else mkPairPositive sizeLower sizeUpper depth


mkPairNegative :: Int -> Int -> IO BisimPair
mkPairNegative sizeLower sizeUpper = do
    (BisimPair _ t1) <- generate (arbitrary :: Gen BisimPair)
    -- (BisimPair _ t2) <- generate (arbitrary :: Gen BisimPair)
    -- let pair = BisimPair t1 t2
    let n1 = nodes t1
    -- let n2 = nodes t2

    if kinded t1 && n1 > sizeLower && n1 <= sizeUpper then do
      t2 <- genOne t1
      return $ BisimPair t1 t2
    else mkPairNegative sizeLower sizeUpper
         
    -- if (n1 > sizeLower && n1 <= sizeUpper) && (n2 > sizeLower && n2 <= sizeUpper) then
    --   if kinded t1 && kinded t2 && not (test pair) then return pair
    --   else mkPairNegative sizeLower sizeUpper
    -- else mkPairNegative sizeLower sizeUpper

  where
    test t1 t2 = B1234.bisimilar $ TG1.convertToGrammar Map.empty [t1, t2]

    genOne t1 = do
      (BisimPair _ t2) <- generate (arbitrary :: Gen BisimPair)
      let n = nodes t2
      if kinded t2 && n > sizeLower && n <= sizeUpper && not (test t1 t2) then return t2
      else genOne t1

main :: IO ()
main = do
  (a:args) <- getArgs
  if read a
    then testBisim args
    else generateTypes args


testBisim :: [String] -> IO ()
testBisim args = do
  let (pair, version, pos) = parseTestArgs args
  runEach pair (bisimCombs !! version) pos

generateTypes :: [String] -> IO ()
generateTypes args = do
  let (sizeLower, sizeUpper, depth, pos) = parseGenArgs args
  if pos then do
    pair <- mkPairPositive sizeLower sizeUpper depth
    printTypes pair
  else do 
    pair <- mkPairNegative sizeLower sizeUpper
    printTypes pair

  where
    printTypes :: BisimPair -> IO ()
    printTypes pair@(BisimPair t1 t2) = do
      let (n1, n2) = nodesOf pair
      putStrLn $ show t1
      putStrLn $ show t2





-- ((rec z:SU.(rec x:SU.(+{A: x, B: !Char};(Skip;(y;y)))));((+{A: x, B: !Char};(Skip;(y;y)));(rec z:SU.(rec y:SU.?Bool))))
-- (((rec z:SU.+{A: (x;((Skip;y);y)), B: (!Char;((Skip;y);y))});+{A: (x;((Skip;y);y)), B: (!Char;((Skip;y);y))});(rec z:SU.?Bool))

-- (rec y:SL.((rec y:SL.z);((rec z:SL.(rec z:SU.(Skip;(?Bool;y))));(+{C: z};y))))
-- (((rec y:SL.z);(rec z:SL.((Skip;?Bool);(rec y:SL.(((rec y:SL.z);(rec z:SL.((Skip;?Bool);y)));+{C: (z;y)})))));+{C: (z;(rec y:SL.(((rec y:SL.z);(rec z:SL.((Skip;?Bool);y)));+{C: (z;y)})))})

-- (rec z:SL.(rec y:SL.(rec y:SL.(+{A: y, B: x, C: x};(rec x:SU.(rec x:SL.z))))))
-- (rec y:SL.+{A: (y;(rec x:SL.(rec z:SL.(rec y:SL.+{A: (y;(rec x:SL.z)), B: (x;(rec x:SL.z)), C: (x;(rec x:SL.z))})))), B: (x;(rec x:SL.(rec z:SL.(rec y:SL.+{A: (y;(rec x:SL.z)), B: (x;(rec x:SL.z)), C: (x;(rec x:SL.z))})))), C: (x;(rec x:SL.(rec z:SL.(rec y:SL.+{A: (y;(rec x:SL.z)), B: (x;(rec x:SL.z)), C: (x;(rec x:SL.z))}))))})

-- (rec y:SL.(&{B: x, C: !Int};(rec x:SU.(rec x:SL.(+{A: y, C: z};?Bool)))))
-- &{B: (x;(rec x:SU.+{A: ((rec y:SL.&{B: (x;(rec x:SU.+{A: (y;?Bool), C: (z;?Bool)})), C: (!Int;(rec x:SU.+{A: (y;?Bool), C: (z;?Bool)}))});?Bool), C: (z;?Bool)})), C: (!Int;(rec x:SU.+{A: ((rec y:SL.&{B: (x;(rec x:SU.+{A: (y;?Bool), C: (z;?Bool)})), C: (!Int;(rec x:SU.+{A: (y;?Bool), C: (z;?Bool)}))});?Bool), C: (z;?Bool)}))}

-- (rec x:SL.((rec z:SU.(rec z:SL.&{B: x, C: ?Bool}));((rec δ:SU.(rec x:SL.!()));(+{B: x, C: z};x))))
-- (((rec z:SL.&{B: (rec x:SL.(((rec z:SL.&{B: x, C: ?Bool});!());+{B: (x;x), C: (z;x)})), C: ?Bool});!());+{B: ((rec x:SL.(((rec z:SL.&{B: x, C: ?Bool});!());+{B: (x;x), C: (z;x)}));(rec x:SL.(((rec z:SL.&{B: x, C: ?Bool});!());+{B: (x;x), C: (z;x)}))), C: (z;(rec x:SL.(((rec z:SL.&{B: x, C: ?Bool});!());+{B: (x;x), C: (z;x)})))})

-- this one hangs even on B1234
-- (Skip;(rec y:SU.(&{A: y, B: !Int};(!Int;(+{A: y, C: x};y)))))
-- ((&{A: (rec y:SU.((&{A: y, B: !Int};!Int);+{A: (y;y), C: (x;y)})), B: !Int};!Int);+{A: ((rec y:SU.((&{A: y, B: !Int};!Int);+{A: (y;y), C: (x;y)}));(rec y:SU.((&{A: y, B: !Int};!Int);+{A: (y;y), C: (x;y)}))), C: (x;(rec y:SU.((&{A: y, B: !Int};!Int);+{A: (y;y), C: (x;y)})))})

-- (rec x:SL.(+{A: (&{C: ?Bool};(&{A: ?Int, C: x};Skip)), C: (&{C: ?Bool};(&{A: ?Int, C: x};Skip))};x))
-- +{A: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};(rec x:SL.+{A: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};x), C: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};x)})), C: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};(rec x:SL.+{A: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};x), C: (&{C: (?Bool;&{A: (?Int;Skip), C: (x;Skip)})};x)}))}

-- (rec y:SU.(&{A: z, C: Skip};(rec z:SL.(rec z:SU.(&{A: x, B: y, C: !Char};x)))))
-- &{A: (z;(rec z:SL.&{A: (x;x), B: ((rec y:SU.&{A: (z;(rec z:SL.&{A: (x;x), B: (y;x), C: (!Char;x)})), C: (Skip;(rec z:SL.&{A: (x;x), B: (y;x), C: (!Char;x)}))});x), C: (!Char;x)})), C: (Skip;(rec z:SL.&{A: (x;x), B: ((rec y:SU.&{A: (z;(rec z:SL.&{A: (x;x), B: (y;x), C: (!Char;x)})), C: (Skip;(rec z:SL.&{A: (x;x), B: (y;x), C: (!Char;x)}))});x), C: (!Char;x)}))}

-- (rec z:SL.(&{B: Skip, C: x};((rec x:SL.(rec x:SL.(+{A: Skip, B: x, C: x};z)));z)))
-- ((&{B: Skip, C: x};(rec x:SL.+{A: (Skip;(rec z:SL.((&{B: Skip, C: x};(rec x:SL.+{A: (Skip;z), B: (x;z), C: (x;z)}));z))), B: (x;(rec z:SL.((&{B: Skip, C: x};(rec x:SL.+{A: (Skip;z), B: (x;z), C: (x;z)}));z))), C: (x;(rec z:SL.((&{B: Skip, C: x};(rec x:SL.+{A: (Skip;z), B: (x;z), C: (x;z)}));z)))}));(rec z:SL.((&{B: Skip, C: x};(rec x:SL.+{A: (Skip;z), B: (x;z), C: (x;z)}));z)))
