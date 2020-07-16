module Interpreter.Builtin where

import qualified Control.Concurrent.Chan as C
import           Data.Char (ord, chr)
import qualified Data.Map as Map
import           Interpreter.Value
import           Syntax.Base
import           Syntax.ProgramVariables

import Debug.Trace

------------------------------------------------------------
-- Communication primitives
------------------------------------------------------------

send :: ChannelEnd -> Value -> IO ChannelEnd
send c v = do
  C.writeChan (snd c) v
  return c

new :: IO Channel
new = do
  ch1 <- C.newChan
  ch2 <- C.newChan
  return ((ch1, ch2), (ch2, ch1))

receive :: ChannelEnd -> IO (Value, ChannelEnd)
receive ch = do
  v <- C.readChan (fst ch)
  return (v, ch)


------------------------------------------------------------  
-- SETUP, builtin functions
------------------------------------------------------------

initialCtx :: Ctx 
initialCtx =
  Map.fromList
  -- Integers
  [ (var "(+)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y)))  
  , (var "(-)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y)))
  , (var "(*)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y)))
  , (var "(/)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "mod", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `mod` y)))
  , (var "rem", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y)))
  , (var "div", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "negate", PrimitiveFun (\(Integer x) -> Integer $ negate x))
  -- Booleans
  , (var "not", PrimitiveFun (\(Boolean x) -> Boolean $ not x))
  , (var "(&&)", PrimitiveFun (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x && y)))
  , (var "(||)", PrimitiveFun (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x || y)))
  , (var "(==)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x == y)))
  , (var "(/=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x /= y)))
  , (var "(<)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x < y)))  
  , (var "(>)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x > y)))  
  , (var "(<=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x <= y)))  
  , (var "(>=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x >= y)))  
  -- Chars
  , (var "chr", PrimitiveFun (\(Integer x) -> Character $ chr x))
  , (var "ord", PrimitiveFun (\(Character x) -> Integer $ ord x))
  -- Prints
  , (var "printInt", PrimitiveFun (\(Integer x) -> IOValue (putStrLn (show x) >> return Unit)))
  , (var "printBool", PrimitiveFun (\(Boolean x) -> IOValue (putStrLn (show x) >> return Unit)))
  , (var "printChar", PrimitiveFun (\(Character x) -> IOValue (putStrLn (show x) >> return Unit)))
  , (var "printUnit", PrimitiveFun (\Unit -> IOValue (putStrLn "()" >> return Unit)))
--  , (var "print", PrimitiveFun (\x -> IOValue (putStrLn (show x) >> return Unit)))
  -- Lists
   , (var "(::)", PrimitiveFun (\x -> PrimitiveFun (\y -> Cons (var "#Cons") (x : [y]))))
   , (var "(++)", PrimitiveFun (\(Cons x xs) -> PrimitiveFun (\y -> Cons x (findNil y xs))))

--   -- new
   , (var "head", PrimitiveFun (\(Cons _ xs) -> head xs))
   , (var "last", PrimitiveFun findLast)
   , (var "tail", PrimitiveFun (\(Cons _ xs) -> last xs))
   , (var "init", PrimitiveFun initList)
-- -- -- (!!) :: [a] -> Int -> a
   , (var "null", PrimitiveFun (\(Cons _ xs) -> Boolean $ null xs))
   , (var "length", PrimitiveFun $ Integer . len)
--   , (var "reverse", fromType listList)
  ]
  where
    var :: String -> ProgVar
    var = mkVar defaultPos


findNil :: Value -> [Value] -> [Value]
findNil _ [] = []
findNil subs ((Cons x xs):ys)
  | null xs   = [subs]
  | null ys   = [Cons x (findNil subs xs)]
  | otherwise = (Cons x xs) : findNil subs ys
findNil subs (x:xs) = x : findNil subs xs


-- Cons [2,Cons [3,Cons [4,Cons [5,Nil []]]]]
findLast :: Value -> Value
findLast (Cons x []) = error $ "last over an empty list"
findLast (Cons y (v:(Cons x xs):ys))
  | x == mkVar defaultPos "#Nil" = v
  | otherwise                    = findLast (Cons y xs) 


initList :: Value -> Value
initList (Cons x []) = error $ "last over an empty list"
initList (Cons y (v:(Cons x xs):_))
  | x == mkVar defaultPos "#Nil" = Cons x xs
  | otherwise                    = Cons y (v : [initList (Cons x xs)]) 

len :: Value -> Int
len (Cons x []) = 0
len (Cons y (v:(Cons x xs):_)) = 1 + len (Cons x xs)

