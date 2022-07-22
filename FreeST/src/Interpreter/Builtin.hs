module Interpreter.Builtin where


import           Interpreter.Value
import           Syntax.Base

import qualified Control.Concurrent.Chan as C
import           Control.Exception ( catch, SomeException )
import           Data.Char ( ord, chr )
import           Data.Functor
import qualified Data.Map as Map
import           System.IO

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
  -- Prints to stderr
  , (var "#printErrValue"  , PrimitiveFun (\v -> IOValue $ hPutStr   stderr (show v) $> Unit))
  , (var "#printErrValueLn", PrimitiveFun (\v -> IOValue $ hPutStrLn stderr (show v) $> Unit))
  -- Reads
  , (var "#readBool", PrimitiveFun (\(Pair (Cons v l) nothingCons) -> genericGet nothingCons $ \s ->
        case reads s of
          [(x, "")] -> Cons v $ l ++ [[Boolean x]]
          _         -> nothingCons
        ))
  , (var "#readInt", PrimitiveFun (\(Pair (Cons v l) nothingCons) -> genericGet nothingCons $ \s ->
        case reads s of
          [(x, "")] -> Cons v $ l ++ [[Integer x]]
          _         -> nothingCons
    ))
  , (var "#readChar", PrimitiveFun (\(Pair (Cons v l) nothingCons) -> genericGet nothingCons $ \s -> 
        case s of
          (c : _) -> Cons v $ l ++ [[Character c]]
          _       -> nothingCons
    ))
  , (var "#readString", PrimitiveFun (\(Pair (Cons v l) nothingCons) -> genericGet nothingCons $ \s -> Cons v $ l ++ [[String s]]))
  -- Files
  , (var "#putFile"
    , PrimitiveFun (\v1 -> 
      PrimitiveFun (\(Cons v2 [[Handle fh]]) ->
      PrimitiveFun (\(Pair okCons errorCons) -> IOValue $
        catchAny
          (hPutStr fh (show v1) >> return okCons)
          (\_ -> return errorCons)
      ))))
  , (var "#closeFile"
    , PrimitiveFun (\(Cons v [[Handle fh]]) -> IOValue $
      catchAny
          (hClose fh >> return Unit)
          (\_ -> return Unit)
      ))
  , (var "#openWriteFile"
    , PrimitiveFun (\(String s) -> 
      PrimitiveFun (\(Cons v1 l1) ->
      PrimitiveFun (\(Pair (Cons v2 l2) nothingCons) -> IOValue $
        catchAny 
          (openFile s WriteMode >>= \handle -> 
           new >>= \(clientChan, serverChan) -> return $
            Cons v2 $ l2 ++ [[Pair (Pair (Chan clientChan) (Chan serverChan)) (Cons v1 (l1 ++ [[Handle handle]]))]]
          )
          (\_ -> return nothingCons)
      ))))
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

  genericRead :: (String -> (Value, Value) -> Value) -> Value
  genericRead f = PrimitiveFun 
    (\(Pair justCons nothingCons) ->
      IOValue $
      catchAny
        (getLine >>= \s -> return $ f s (justCons, nothingCons)) 
        (\_ -> return nothingCons)
    )

  genericGet :: Value -> (String -> Value) -> Value
  genericGet nothingCons f = 
    IOValue $
    catchAny
      (getLine >>= \s -> return $ f s) 
      (\_ -> return nothingCons)
