module Interpreter.Builtin where

import qualified Control.Concurrent.Chan       as C
import           Data.Char                      ( ord
                                                , chr
                                                )
import qualified Data.Map                      as Map
import           Interpreter.Value
import           Syntax.Base
import           Syntax.ProgramVariable


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
    ( var "send",
      PrimitiveFun
        (\v-> PrimitiveFun (\(Chan c) -> IOValue $ fmap Chan (send v c)))
    )
  ,

    ( var "receive",
      PrimitiveFun
        (\(Chan c) -> IOValue $ receive c >>= \(v, c) -> return $ Pair v (Chan c))
    )
  ,
    -- ( var "fork",
    --   PrimitiveFun
    --     (\v -> IOValue $ forkIO $ void $ eval ctx eenv e -> return Unit)
    -- )
--  ,
    ( var "(+)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y))
    )
  , ( var "(-)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y))
    )
  , ( var "(*)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y))
    )
  , ( var "(/)"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y))
    )
  , ( var "mod"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `mod` y))
    )
  , ( var "rem"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y))
    )
  , ( var "div"
    , PrimitiveFun
      (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y))
    )
  , ( var "negate"
    , PrimitiveFun (\(Integer x) -> Integer $ negate x)
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
  , (var "snd", PrimitiveFun (\(Pair _ b) -> b))
  -- Prints
  , ( var "printInt"
    , PrimitiveFun (\(Integer x) -> IOValue (putStr (show x) >> return Unit))
    )
  , ( var "printIntLn"
    , PrimitiveFun (\(Integer x) -> IOValue (print x >> return Unit))
    )
  , ( var "printBool"
    , PrimitiveFun (\(Boolean x) -> IOValue (putStr (show x) >> return Unit))
    )
  , ( var "printBoolLn"
    , PrimitiveFun (\(Boolean x) -> IOValue (print x >> return Unit))
    )
  , ( var "printChar"
    , PrimitiveFun (\(Character x) -> IOValue (putStr (show x) >> return Unit))
    )
  , ( var "printCharLn"
    , PrimitiveFun (\(Character x) -> IOValue (print x >> return Unit))
    )
  , ( var "printUnit"
    , PrimitiveFun (\Unit -> IOValue (putStr "()" >> return Unit))
    )
  , ( var "printUnitLn"
    , PrimitiveFun (\Unit -> IOValue (putStrLn "()" >> return Unit))
    )
  , ( var "printString"
    , PrimitiveFun (\(String s) -> IOValue (putStr s >> return Unit))
    )
  , ( var "printStringLn"
    , PrimitiveFun (\(String s) -> IOValue (putStrLn s >> return Unit))
    )
--  , (var "print", PrimitiveFun (\x -> IOValue (putStrLn (show x) >> return Unit)))
  ]
 where
  var :: String -> ProgVar
  var = mkVar defaultPos
