import qualified OldBisim as OB
import qualified NewBisim as NB


--import qualified TypeToGrammar as TG
--import qualified TypeToGrammar1 as TG1
import qualified TypeToGrammar3 as TG3

import Parse.Parser
import Parse.Read

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Random
import Test.QuickCheck.Gen
import ArbitraryTypes
import qualified Data.Map.Strict as Map


-- Freest
import           Kinding.Kinding
import           Syntax.Type
--import           Syntax.Schemes
--import           Syntax.Kinds
--import           Syntax.TypeVariables
import           Syntax.Base
import           Elaboration.Duality
--import           Utils.FreestState
import           Control.Monad.State
import           Typing.Rename
import           Debug.Trace


import           System.Timeout
import           System.Random
import           System.Environment
import           System.IO
import           System.Clock
import           Formatting
import           Formatting.Clock

import           Control.Exception
import Syntax.Base (defaultSpan)

--type BisimFunction = (TypeEnv -> Type -> Type -> Bool)
-- 1m
timeoutInMicro = 2 * 60 * 1000000

-- Bisim functions combinations
-- bisimCombs :: [(String, TypeEnv -> Type -> Type -> Bool)]
bisimCombs = -- Map.fromList
  [ ("OB", OB.bisimilarGrm, TG3.convertToGrammar)
  , ("NB", NB.isBisimilar, TG3.convertToGrammar)
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
defaultType = Skip defaultSpan
  
runEach pair (name, f, g) pos = do
  v <-  timeout timeoutInMicro $ runTestVersion pair f g

  let (n1, n2) = nodesOf pair
  let base = name ++ ";"  ++ n1 ++ ";" ++ n2 ++ ";" ++ (show pos) ++ ";"
  let (BisimPair t1 t2) = pair
  case v of
    Nothing -> putStrLn $ base ++ "timeout"
    (Just (time, r)) -> putStrLn $ base ++ (show r) ++ ";" ++ time


-- runTestVersion :: BisimPair -> String -> IO (String, Bool)
runTestVersion (BisimPair t u) f g = do
    res <- clockSomething (f $ g [t, u])
    return res


-- QuickCheck

--pos :: Pos
--pos = defaultPos

nodes :: Type -> Int
nodes (Semi _ t u)   = 1 + nodes t + nodes u
nodes (Labelled _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (Rec _ (Bind _ _ _ t))  = 1 + nodes t
nodes _              = 1 -- Skip, Message, TypeVar

nodesOf :: BisimPair -> (String, String)
nodesOf (BisimPair t1 t2) = (show $ nodes t1, show $nodes t2)

--kindEnv :: KindEnv
--kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))

--kinded :: Type -> Bool
--kinded t = null (errors s)
--  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")


parseGenArgs :: [String] -> (Int,Int,Int,Bool)
parseGenArgs [] = (0,0,0,True)  
parseGenArgs (sizeLower:sizeUpper:depth:pos:_) = (read sizeLower, read sizeUpper, read depth, read pos)   

-- Gen positive types
--mkPairPositive ::  Int -> Int -> IO BisimPair
--mkPairPositive sizeLower sizeUpper = do
--    (BisimPair t1 t2) <- generate (arbitrary :: Gen BisimPair) -- generator depth

--    let n1 = nodes t1
--    let n2 = nodes t2
    
--    if (n1 > sizeLower && n1 <= sizeUpper) || (n2 > sizeLower && n2 <= sizeUpper) then
--      if kinded t1 && kinded t2 then
--        return (BisimPair t1 t2)
--      else mkPairPositive sizeLower sizeUpper depth
--    else mkPairPositive sizeLower sizeUpper

-- mkPairNegative ::  Int -> Int -> IO NonBisimPair
-- mkPairNegative sizeLower sizeUpper = do
--     (NonBisimPair t1 t2) <- generate (arbitrary :: Gen NonBisimPair)
--     sizedPair t1 t2
--   where
--     sizedPair t1 t2
--       | nodes t1 > sizeLower && nodes t1 <= sizeUpper =
--           return (NonBisimPair t1 t2)
--       | nodes t2 > sizeLower && nodes t2 <= sizeUpper =
--           return (NonBisimPair t1 t2)
--       | otherwise                         =
--           mkPairNegative sizeLower sizeUpper

--mkPairNegative ::  Int -> Int -> IO NonBisimPair
--mkPairNegative sizeLower sizeUpper = do
--    (NonBisimPair t1 t2) <- generate (arbitrary :: Gen NonBisimPair)
--    if (inInterval t1 || inInterval t2) && not (testBisim t1 t2)
--        (kinded t1 && kinded t2) && 
--      then return (NonBisimPair t1 t2)
--      else mkPairNegative sizeLower sizeUpper 
    -- if (inInterval t1 || inInterval t2) && (kinded t1 && kinded t2)
    --   then do
    --     b <- timeout (3 * 60 * 1000000) $ testBisim t1 t2
    --     case b of
    --       Nothing -> mkPairNegative sizeLower sizeUpper
    --       Just b1 ->
    --         if not b1
    --         then return (NonBisimPair t1 t2)
    --         else mkPairNegative sizeLower sizeUpper
    --   else mkPairNegative sizeLower sizeUpper
      
--  where
--    testBisim t1 t2 = -- return $ 
--      B4.bisimilar $ TG1.convertToGrammar Map.empty [t1, t2] -- B14

--    inInterval t = nodes t > sizeLower && nodes t <= sizeUpper

   

-- mkPairNegative :: Int -> Int -> IO BisimPair
-- mkPairNegative sizeLower sizeUpper = do
--     (BisimPair _ t1) <- generate (arbitrary :: Gen BisimPair)
--     -- (BisimPair _ t2) <- generate (arbitrary :: Gen BisimPair)
--     -- let pair = BisimPair t1 t2
--     let n1 = nodes t1
--     -- let n2 = nodes t2

--     if kinded t1 && n1 > sizeLower && n1 <= sizeUpper then do
--       t2 <- genOne t1
--       return $ BisimPair t1 t2
--     else mkPairNegative sizeLower sizeUpper
--   where
--     test t1 t2 = B1234.bisimilar $ TG1.convertToGrammar Map.empty [t1, t2]

--     genOne t1 = do
--       (BisimPair _ t2) <- generate (arbitrary :: Gen BisimPair)
--       let n = nodes t2
--       if kinded t2 && n > sizeLower && n <= sizeUpper && not (test t1 t2) then return t2
--       else genOne t1

main :: IO ()
main = do
  (a:args) <- getArgs
  testBisim args
   -- else generateTypes args


testBisim :: [String] -> IO ()
testBisim args = do
  let (pair, version, pos) = parseTestArgs args
  runEach pair (bisimCombs !! version) pos

--generateTypes :: [String] -> IO ()
--generateTypes args = do
--  let (sizeLower, sizeUpper, _, pos) = parseGenArgs args
--  if pos then do
--    pair <- mkPairPositive sizeLower sizeUpper
--    printBisimPairs pair
--  else do 
--    pair <- mkPairNegative sizeLower sizeUpper
--    printNonBisimPairs pair

--  where
--    printBisimPairs :: BisimPair -> IO ()
--    printBisimPairs (BisimPair t1 t2) = do
--      putStrLn $ show t1
 --     putStrLn $ show t2

 --   printNonBisimPairs :: NonBisimPair -> IO ()
  --  printNonBisimPairs (NonBisimPair t1 t2) = do
   --   putStrLn $ show t1
    --  putStrLn $ show t2




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
