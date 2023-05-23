module Interpreter.Builtin where


import           Interpreter.Value
import           Syntax.Base

import qualified Control.Concurrent.Chan as C
import           Control.Exception ( catch, SomeException )
import           Data.Char ( ord, chr )
import           Data.Functor
import qualified Data.Map as Map
import           System.IO
import           System.IO.Unsafe
import           Data.Bifunctor (Bifunctor(bimap))

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

close :: ChannelEnd -> IO Value 
close ch = do 
  C.writeChan (snd ch) Unit 
  C.readChan (fst ch)
  return Unit

------------------------------------------------------------  
-- SETUP, builtin functions
------------------------------------------------------------

initialCtx :: Ctx
initialCtx = Map.fromList
  -- Integers
  [ -- Communication primitives
    (var "new", PrimitiveFun (\_ -> IOValue $ uncurry Pair <$> (bimap Chan Chan <$> new)))
  , (var "send", PrimitiveFun (\v -> PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send v c)))
  , (var "receive", PrimitiveFun (\(Chan c) -> IOValue $ receive c >>= \(v, c) -> return $ Pair v (Chan c)))
  , (var "close", PrimitiveFun (\(Chan c) -> IOValue $ close c))
  -- Integer
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
  , (var "even", PrimitiveFun (\(Integer x) -> boolean $ even x))
  , (var "odd" , PrimitiveFun (\(Integer x) -> boolean $ odd x))
  , (var "gcd", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `gcd` y)))
  , (var "lcm", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `lcm` y)))
  -- Double
  , (var "(+.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x + y)))
  , (var "(-.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x - y)))
  , (var "(*.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x * y)))
  , (var "(>.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x > y)))
  , (var "(<.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x < y)))
  , (var "(<=.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x <= y)))
  , (var "(>=.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x >= y)))
  , (var "minF", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x `min` y)))
  , (var "maxF", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x `max` y)))

  -- Booleans
  , (var "(&&)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) && read (show y))))
  , (var "(||)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) || read (show y))))
  , (var "(==)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x == y)))
  , (var "(/=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x /= y)))
  , (var "(<)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x <  y)))
  , (var "(>)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x >  y)))
  , (var "(<=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x <= y)))
  , (var "(>=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x >= y)))
  -- Chars
  , (var "chr", PrimitiveFun (\(Integer x) -> Character $ chr x))
  , (var "ord", PrimitiveFun (\(Character x) -> Integer $ ord x))
  -- Strings
  , (var "(++)", PrimitiveFun (\(String s1) -> PrimitiveFun (\(String s2) -> String $ s1 ++ s2)))
  -- Show
  , (var "show", PrimitiveFun (String . show))
  -- Read
  , (var "readBool", PrimitiveFun (\(String s) -> boolean (read s)))
  , (var "readInt" , PrimitiveFun (\(String s) -> Integer (read s)))
  , (var "readChar", PrimitiveFun (\(String (c : _)) -> Character c))
  -- Print to stdout
  , (var "__putStrOut", PrimitiveFun (\v -> IOValue $ putStr (show v) $> Unit))
  -- Print to stderr
  , (var "__putStrErr", PrimitiveFun (\v -> IOValue $ hPutStr stderr (show v) $> Unit))
  -- Read from stdin
  , (var "__getChar", PrimitiveFun (\_ -> IOValue $ getChar >>= (return . Character)))
  , (var "__getLine", PrimitiveFun (\_ -> IOValue $ getLine >>= (return . String)))
  -- Files
  , (var "__openFile",
      PrimitiveFun (\(String s) ->
      PrimitiveFun (\(Cons (Variable _ mode) _) -> IOValue $
        case mode of
          "ReadMode"   -> openFile s ReadMode   >>= return . Cons (var "FileHandle") . (: []) . (: []) . Handle
          "WriteMode"  -> openFile s WriteMode  >>= return . Cons (var "FileHandle") . (: []) . (: []) . Handle
          "AppendMode" -> openFile s AppendMode >>= return . Cons (var "FileHandle") . (: []) . (: []) . Handle
    )))
  , (var "__putFileStr",
      PrimitiveFun (\(Cons _ [[Handle fh]]) -> PrimitiveFun (\(String s) ->
        IOValue $ hPutStr fh s $> Unit
    )))
  , (var "__readFileChar", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetChar fh >>= return . Character))
  , (var "__readFileLine", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetLine fh >>= return . String))
  , (var "__isEOF"       , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hIsEOF fh >>= return . boolean))
  , (var "__closeFile"   , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hClose fh $> Unit))
  -- Id  
  , (var "id", PrimitiveFun id)
  -- Undefined
--  , (var "undefined", PrimitiveFun undefined)
  -- Error
  -- , (var "error", PrimitiveFun (\(String e) ->
  --        unsafePerformIO $ die $ showErrors "" Map.empty [] (ErrorFunction s e)))
  ]
 where
  var :: String -> Variable
  var = mkVar defaultSpan
  boolean :: Bool -> Value
  boolean b = Cons (mkVar defaultSpan (show b)) []
