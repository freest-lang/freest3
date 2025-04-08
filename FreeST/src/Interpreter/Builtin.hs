module Interpreter.Builtin where


import           Interpreter.Value
import           Syntax.Base

import qualified Control.Concurrent.Chan as C
import           Data.Char ( ord, chr )
import           Data.Functor
import qualified Data.Map as Map
import           System.IO
import           Data.Bifunctor (Bifunctor(bimap))
-- import Numeric (Floating(log1p, expm1, log1pexp, log1mexp))
import GHC.Float
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Bits

------------------------------------------------------------
-- Communication primitives
------------------------------------------------------------

new :: IO Channel
new = do
  c1 <- C.newChan
  c2 <- C.newChan
  return ((c1, c2), (c2, c1))

receive :: ChannelEnd -> IO (Value, ChannelEnd)
receive c = do
  v <- C.readChan (fst c)
  return (v, c)

send :: Value -> ChannelEnd -> IO ChannelEnd
send v c = do
  C.writeChan (snd c) v
  return c

wait :: Value -> Value
wait (Chan c) =
  IOValue $ C.readChan (fst c)

close :: Value -> IO Value
close (Chan c) = do
  C.writeChan (snd c) Unit
  return Unit

------------------------------------------------------------  
-- SETUP, builtin functions
------------------------------------------------------------

initialCtx :: Ctx
initialCtx = Map.fromList
  [ -- Communication primitives
    (var "new", PrimitiveFun (\_ -> IOValue $ uncurry Pair <$> (bimap Chan Chan <$> new)))
  , (var "receive", PrimitiveFun (\(Chan c) -> IOValue $ receive c >>= \(v, c) -> return $ Pair v (Chan c)))
  , (var "send", PrimitiveFun (\v -> PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send v c)))
  , (var "wait", PrimitiveFun wait)
  , (var "close", PrimitiveFun (IOValue . close))
  -- Integer
  , (var "(+)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x + y)))
  , (var "(-)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x - y)))
  , (var "subtract", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ y - x)))
  , (var "(*)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x * y)))
  , (var "(/)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `div` y)))
  , (var "(^)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x ^ y)))
  , (var "abs", PrimitiveFun (\(Int x) -> Int $ abs x))
  , (var "mod", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `mod` y)))
  , (var "rem", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `rem` y)))
  -- if we add fractional numbers, this is the integer division, now used as (/)    
  , (var "div", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `div` y)))
  , (var "negate", PrimitiveFun (\(Int x) -> Int $ negate x))
  , (var "max", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `max` y)))
  , (var "min", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `min` y)))
  , (var "succ", PrimitiveFun (\(Int x) -> Int $ succ x))
  , (var "pred", PrimitiveFun (\(Int x) -> Int $ pred x))
  , (var "abs" , PrimitiveFun (\(Int x) -> Int $ abs x))
  , (var "quot", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `quot` y)))
  , (var "even", PrimitiveFun (\(Int x) -> boolean $ even x))
  , (var "odd" , PrimitiveFun (\(Int x) -> boolean $ odd x))
  , (var "gcd", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `gcd` y)))
  , (var "lcm", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x `lcm` y)))
  , (var "shiftL", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ shiftL x y)))
  , (var "shiftR", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ shiftR x y)))
  , (var "land", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x .&. y)))
  , (var "lor", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ x .|. y)))
  , (var "lxor", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> Int $ xor x y)))
  -- Float
  , (var "(+.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x + y)))
  , (var "(-.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x - y)))
  , (var "(*.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x * y)))
  , (var "(/.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x / y)))
  , (var "(>.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x > y)))
  , (var "(<.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x < y)))
  , (var "(<=.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x <= y)))
  , (var "(>=.)", PrimitiveFun (\(Float x) -> PrimitiveFun (\(Float y) -> boolean $ x >= y)))
  , (var "absF", PrimitiveFun (\(Float x) -> Float $ abs x))
  , (var "negateF", PrimitiveFun (\(Float x) -> Float $ negate x))
  , (var "minF", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x `min` y)))
  , (var "maxF", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x `max` y)))
  , (var "truncate", PrimitiveFun (\(Float x) -> Int $ truncate x))
  , (var "round", PrimitiveFun (\(Float x) -> Int $ round x))
  , (var "ceiling", PrimitiveFun (\(Float x) -> Int $ ceiling x))
  , (var "floor", PrimitiveFun (\(Float x) -> Int $ floor x))
  , (var "recip" , PrimitiveFun (\(Float x) -> Float $ recip x))
  , (var "pi", Float pi )
  , (var "exp", PrimitiveFun (\(Float x) -> Float $ exp x))
  , (var "log", PrimitiveFun (\(Float x) -> Float $ log x))
  , (var "sqrt", PrimitiveFun (\(Float x) -> Float $ sqrt x))
  , (var "(**)", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ x ** y)))
  , (var "logBase", PrimitiveFun(\(Float x) -> PrimitiveFun (\(Float y) -> Float $ logBase x y)))
  , (var "sin", PrimitiveFun (\(Float x) -> Float $ sin x))
  , (var "cos", PrimitiveFun (\(Float x) -> Float $ cos x))
  , (var "tan", PrimitiveFun (\(Float x) -> Float $ tan x))
  , (var "asin", PrimitiveFun (\(Float x) -> Float $ asin x))
  , (var "acos", PrimitiveFun (\(Float x) -> Float $ acos x))
  , (var "atan", PrimitiveFun (\(Float x) -> Float $ atan x))
  , (var "sinh", PrimitiveFun (\(Float x) -> Float $ sinh x))
  , (var "cosh", PrimitiveFun (\(Float x) -> Float $ cosh x))
  , (var "tanh", PrimitiveFun (\(Float x) -> Float $ tanh x))
  , (var "log1p", PrimitiveFun (\(Float x) -> Float $ log1p x))
  , (var "expm1", PrimitiveFun (\(Float x) -> Float $ expm1 x))
  , (var "log1pexp", PrimitiveFun (\(Float x) -> Float $ log1pexp x))
  , (var "log1mexp", PrimitiveFun (\(Float x) -> Float $ log1mexp x))
  , (var "fromInteger", PrimitiveFun (\(Int x) -> Float $ Prelude.fromInteger (toInteger x)))
  -- Infinite Percision Integer
  , (var "(+i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y)))
  , (var "(-i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y)))
  , (var "subtractI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ y - x)))
  , (var "(*i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y)))
  , (var "(/i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "(^i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x ^ y)))
  , (var "absI", PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , (var "modI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `mod` y)))
  , (var "remI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y)))
  , (var "divI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "negateI", PrimitiveFun (\(Integer x) -> Integer $ negate x))
  , (var "maxI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `max` y)))
  , (var "minI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `min` y)))
  , (var "succI", PrimitiveFun (\(Integer x) -> Integer $ succ x))
  , (var "predI", PrimitiveFun (\(Integer x) -> Integer $ pred x))
  , (var "absI" , PrimitiveFun (\(Integer x) -> Integer $ abs x))
  , (var "quotI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `quot` y)))
  , (var "evenI", PrimitiveFun (\(Integer x) -> boolean $ even x))
  , (var "oddI" , PrimitiveFun (\(Integer x) -> boolean $ odd x))
  , (var "gcdI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `gcd` y)))
  , (var "lcmI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `lcm` y)))
  , (var "(==i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x == y)))
  , (var "(/=i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x /= y)))
  , (var "(>i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x > y)))
  , (var "(<i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x < y)))
  , (var "(<=i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x <= y)))
  , (var "(>=i)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> boolean $ x >= y)))
  , (var "shiftLI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Int y) -> Integer $ shiftL x y)))
  , (var "shiftRI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Int y) -> Integer $ shiftR x y)))
  , (var "landI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x .&. y)))
  , (var "lorI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x .|. y)))
  , (var "lxorI", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ xor x y)))
  , (var "intToInteger", PrimitiveFun (\(Int x) -> Integer $ toInteger x))
  , (var "integerToInt", PrimitiveFun (\(Integer x) -> Int $ fromIntegral x))
  -- Booleans
  , (var "(&&)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) && read (show y))))
  , (var "(||)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) || read (show y))))
  , (var "(==)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x == y)))
  , (var "(/=)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x /= y)))
  , (var "(<)" , PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x <  y)))
  , (var "(>)" , PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x >  y)))
  , (var "(<=)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x <= y)))
  , (var "(>=)", PrimitiveFun (\(Int x) -> PrimitiveFun (\(Int y) -> boolean $ x >= y)))
  -- Chars
  , (var "chr", PrimitiveFun (\(Int x) -> Character $ chr x))
  , (var "ord", PrimitiveFun (\(Character x) -> Int $ ord x))
  -- Strings
  , (var "(^^)", PrimitiveFun (\(String s1) -> PrimitiveFun (\(String s2) -> String $ s1 ++ s2)))
  -- Show
  , (var "show", PrimitiveFun (String . show))
  -- Read
  , (var "readBool", PrimitiveFun (\(String s) -> boolean (read s)))
  , (var "readInt" , PrimitiveFun (\(String s) -> Int (read s)))
  , (var "readInteger" , PrimitiveFun (\(String s) -> Integer (read s)))
  , (var "readChar", PrimitiveFun (\(String (c : _)) -> Character c))
  -- Print to stdout
  , (var "__putStrOut", PrimitiveFun (\v -> IOValue $ putStr (show v) $> Unit))
  -- Print to stderr
  , (var "__putStrErr", PrimitiveFun (\v -> IOValue $ hPutStr stderr (show v) $> Unit))
  -- Read from stdin
  , (var "__getChar"    , PrimitiveFun (\_ -> IOValue $ getChar     <&> Character))
  , (var "__getLine"    , PrimitiveFun (\_ -> IOValue $ getLine     <&> String   ))
  , (var "__getContents", PrimitiveFun (\_ -> IOValue $ getContents <&> String   ))
  -- Files
  , (var "__openFile",
      PrimitiveFun (\(String s) ->
      PrimitiveFun (\(Cons (Variable _ mode _) _) -> IOValue $
        case mode of
          "ReadMode"   -> openFile s ReadMode <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
          "WriteMode"  -> openFile s WriteMode  <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
          "AppendMode" -> openFile s AppendMode <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
          xs -> error $ "Interpreter.Builtin.initialCtx " ++ show xs
    )))
  , (var "__putFileStr",
      PrimitiveFun (\(Cons _ [[Handle fh]]) -> PrimitiveFun (\(String s) ->
        IOValue $ hPutStr fh s $> Unit
    )))
  , (var "__readFileChar", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetChar fh <&> Character))
  , (var "__readFileLine", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetLine fh <&> String))
  , (var "__isEOF"       , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hIsEOF fh <&> boolean))
  , (var "__closeFile"   , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hClose fh $> Unit))
  -- Id  
  , (var "id", PrimitiveFun id)
  -- System time (seed for rng)
  , (var "getSystemTime", PrimitiveFun (\_ -> IOValue $ fmap (Int . truncate . (* 10^9)) getPOSIXTime))
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
