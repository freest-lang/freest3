module Interpreter.Builtin where


import           Interpreter.Value
import           Syntax.Base

import qualified Control.Concurrent.Chan as C
import           Control.Exception ( catch, SomeException )
import           Data.Char ( ord, chr )
import           Data.Functor
import qualified Data.Map as Map
import Debug.Trace
------------------------------------------------------------
-- Communication primitives
------------------------------------------------------------

send :: Value -> ChannelEnd -> IO ChannelEnd
send v c = do
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
initialCtx = Map.fromList
  -- Integers
 [ -- Communication primitives
    (var "send", PrimitiveFun (\v -> PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send v c)))
  , (var "receive", PrimitiveFun (\(Chan c) -> IOValue $ receive c >>= \(v, c) -> return $ Pair v (Chan c)))
  -- Integers
  , (var "(+)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y)))
  , (var "(-)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y)))
  , (var "subtract", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ y - x)))
  , (var "(*)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y)))
  , (var "(/)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "(^)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x ^ y)))
  , (var "abs", PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , (var "mod", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `mod` y)))
  , (var "rem", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y)))
  -- if we add fractional numbers, this is the integer division, now used as (/)    
  , (var "div", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "negate", PrimitiveFun (\(Integer x) -> Integer $ negate x))
  , (var "max", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `max` y)))
  , (var "min", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `min` y)))
  , (var "succ", PrimitiveFun (\(Integer x) -> Integer $ succ x))
  , (var "pred", PrimitiveFun (\(Integer x) -> Integer $ pred x))
  , (var "abs" , PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , (var "quot", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `quot` y)))
  , (var "even", PrimitiveFun (\(Integer x) -> Boolean $ even x))
  , (var "odd" , PrimitiveFun (\(Integer x) -> Boolean $ odd x))
  , (var "gcd", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `gcd` y)))
  , (var "lcm", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `lcm` y)))
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
  -- Pairs
  , (var "fst", PrimitiveFun (\(Pair a _) -> a))
  , (var "snd", PrimitiveFun (\(Pair _ b) -> b))
  -- Prints
  , (var "#printValue"  , PrimitiveFun (\v -> IOValue $ putStr   (show v) $> Unit))
  , (var "#printValueLn", PrimitiveFun (\v -> IOValue $ putStrLn (show v) $> Unit))
  , (var "#readBool", genericRead (\s -> Boolean (read s)))
  , (var "#readInt", genericRead  (\s -> Integer (read s)))
  , (var "#readChar", genericRead (\(c : s) -> Character c))
  , (var "#readString", genericRead String)
  -- Id  
  , (var "id", PrimitiveFun id)
  -- Undefined
--  , (var "undefined", PrimitiveFun undefined)
  -- Error
  -- , (var "error", PrimitiveFun (\(String e) ->
  --        unsafePerformIO $ die $ showErrors "" Map.empty [] (ErrorFunction s e)))

--  , (var "print", PrimitiveFun (\x -> IOValue (putStrLn (show x) >> return Unit)))
  ]
 where
  var :: String -> Variable
  var = mkVar defaultSpan

  catchAny :: IO a -> (SomeException -> IO a) -> IO a
  catchAny = catch

  genericRead :: (String -> Value) -> Value
  genericRead f = PrimitiveFun 
    (\(Pair (Cons v l) nothingCons) ->
      IOValue $
      catchAny
        (getLine >>= \s -> return $ Cons v $ l ++ [[f s]]) 
        (\_ -> return nothingCons)
    )
