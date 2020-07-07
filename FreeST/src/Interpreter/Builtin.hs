module Interpreter.Builtin where

import qualified Control.Concurrent.Chan as C
import           Data.Char (ord, chr)
import qualified Data.Map as Map
import           Interpreter.Value
import           Syntax.Base
import           Syntax.ProgramVariables

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
  , (var "(::)", PrimitiveFun (\x -> PrimitiveFun (\y -> Cons (var "#Cons") ([[x]] ++ [[y]]))))
  , (var "(++)", PrimitiveFun (\(Cons x xs) -> PrimitiveFun (\y -> Cons x (findNil y xs))))
  ]
  where
    var :: String -> ProgVar
    var = mkVar defaultPos


findNil :: Value -> [[Value]]-> [[Value]]
findNil subs = map (findNil' subs)

findNil' :: Value -> [Value] -> [Value]
findNil' subs = map (findNil'' subs)

findNil'' :: Value -> Value -> Value
findNil'' subs (Cons x xs)
  | x == nil = subs
  | otherwise = Cons x (findNil subs xs)
findNil'' _ x = x

nil = mkVar defaultPos "#Nil"

