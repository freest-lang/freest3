module Interpreter.Builtin where

import qualified Control.Concurrent.Chan       as C
import           Data.Char                      ( ord
                                                , chr
                                                )
import qualified Data.Map                      as Map
import           Interpreter.Value
import           Syntax.Base
import           Syntax.ProgramVariable         ( ProgVar )
import           System.Exit                    ( die )
import           System.IO.Unsafe               ( unsafePerformIO )

import           Util.Error                     ( formatErrorMessages )
import           Util.ErrorMessage              ( ErrorMessage(..) )
import           Data.Functor

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
    ( var "send"
    , PrimitiveFun
      (\v -> PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send v c))
    )
  , ( var "receive"
    , PrimitiveFun
      (\(Chan c) -> IOValue $ receive c >>= \(v, c) -> return $ Pair v (Chan c))
    )
  -- Integers
  , ( var "(+)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y))
    )
  , ( var "(-)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y))
    )
  , ( var "subtract"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ y - x))
    )
  , ( var "(*)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y))
    )
  , ( var "(/)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y))
    )
  , ( var "(^)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x ^ y))
    )    
  , (var "abs", PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , ( var "mod"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `mod` y))
    )
  , ( var "rem"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y))
    )
  -- if we add fractional numbers, this is the integer division, now used as (/)    
  , ( var "div"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y))
    )
  , (var "negate", PrimitiveFun (\(Integer x) -> Integer $ negate x))

  , ( var "max"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `max` y))
    )
  , ( var "min"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `min` y))
    )
  , (var "succ", PrimitiveFun (\(Integer x) -> Integer $ succ x))
  , (var "pred", PrimitiveFun (\(Integer x) -> Integer $ pred x))
  
  , (var "abs" , PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , ( var "quot"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `quot` y))
    )
  , (var "even", PrimitiveFun (\(Integer x) -> Boolean $ even x))
  , (var "odd" , PrimitiveFun (\(Integer x) -> Boolean $ odd x))
  , ( var "gcd"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `gcd` y))
    )
  , ( var "lcm"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `lcm` y))
    )
  -- Booleans
  , (var "not", PrimitiveFun (\(Boolean x) -> Boolean $ not x))
  , ( var "(&&)"
    , PrimitiveFun
      (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x && y))
    )
  , ( var "(||)"
    , PrimitiveFun
      (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x || y))
    )
  , ( var "(==)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x == y))
    )
  , ( var "(/=)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x /= y))
    )
  , ( var "(<)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x < y))
    )
  , ( var "(>)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x > y))
    )
  , ( var "(<=)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x <= y))
    )
  , ( var "(>=)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x >= y))
    )
  -- Chars
  , (var "chr", PrimitiveFun (\(Integer x) -> Character $ chr x))
  , ( var "ord"
    , PrimitiveFun (\(Character x) -> Integer $ ord x)
    )
  -- Pairs
  , (var "fst", PrimitiveFun (\(Pair a _) -> a))
  , ( var "snd", PrimitiveFun (\(Pair _ b) -> b))
  -- Prints
  , ( var "printInt"
    , PrimitiveFun (\(Integer x) -> IOValue $ putStr (show x) $> Unit)
    )
  , (var "printIntLn", PrimitiveFun (\(Integer x) -> IOValue $ print x $> Unit))
  , ( var "printBool"
    , PrimitiveFun (\(Boolean x) -> IOValue $ putStr (show x) $> Unit)
    )
  , ( var "printBoolLn"
    , PrimitiveFun (\(Boolean x) -> IOValue $ print x $> Unit)
    )
  , ( var "printChar"
    , PrimitiveFun (\(Character x) -> IOValue $ putStr (show x) $> Unit)
    )
  , ( var "printCharLn"
    , PrimitiveFun (\(Character x) -> IOValue $ print x $> Unit)
    )
  , (var "printUnit"  , PrimitiveFun (\Unit -> IOValue $ putStr "()" $> Unit))
  , (var "printUnitLn", PrimitiveFun (\Unit -> IOValue $ putStrLn "()" $> Unit))
  , ( var "printString"
    , PrimitiveFun (\(String s) -> IOValue $ putStr s $> Unit)
    )
  , ( var "printStringLn"
    , PrimitiveFun (\(String s) -> IOValue $ putStrLn s $> Unit)
    )
  -- Id  
  , ( var "id"
    , PrimitiveFun id
    )
  -- Error
  , ( var "error"
    , PrimitiveFun
      (\(String s) -> unsafePerformIO $ die $ formatErrorMessages Map.empty
                                                                  defaultPos
                                                                  "FreeST"
                                                                  [Error s]
      )
    )

--  , (var "print", PrimitiveFun (\x -> IOValue (putStrLn (show x) >> return Unit)))
  ]
 where
  var :: String -> ProgVar
  var = mkVar defaultPos
