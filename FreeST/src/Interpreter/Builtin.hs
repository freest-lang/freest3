module Interpreter.Builtin where


import           Interpreter.Value
import           Syntax.Base
import           Syntax.MkName

import qualified Control.Concurrent.Chan as C
import           Control.Exception ( catch, SomeException )
import           Data.Char ( ord, chr )
import           Data.Functor
import qualified Data.Map as Map
import           System.IO
import           System.IO.Unsafe
import           Data.Bifunctor (Bifunctor(bimap))
import Numeric (Floating(log1p, expm1, log1pexp, log1mexp))
import GHC.Float

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
  C.writeChan (snd ch) (BasicT Unit) 
  C.readChan (fst ch)
  return $ BasicT Unit

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
  , (var "(+)", -- PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x + y)))
      PrimitiveFun (\x -> PrimitiveFun (\y -> case x of
                     (BasicT (Integer x')) -> let (BasicT (Integer y')) = y in BasicT $ Integer $ x' + y'
                     (BasicT (Float x')) -> let (BasicT (Float y')) = y in BasicT $ Float $ x' + y')))
  , (var "(-)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x - y)))
  , (var "subtract", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ y - x)))
  , (var "(*)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x * y)))
  , (var "(/)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `div` y)))
  , (var "(^)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x ^ y)))
  , (var "abs", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Integer $ abs x))
  , (var "mod", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `mod` y)))
  , (var "rem", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `rem` y)))
  -- if we add fractional numbers, this is the integer division, now used as (/)    
  , (var "div", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `div` y)))
  , (var "negate", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Integer $ negate x))
  , (var "max", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `max` y)))
  , (var "min", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `min` y)))
  , (var "succ", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Integer $ succ x))
  , (var "pred", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Integer $ pred x))
  , (var "abs" , PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Integer $ abs x))
  , (var "quot", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `quot` y)))
  , (var "even", PrimitiveFun (\(BasicT (Integer x)) -> boolean $ even x))
  , (var "odd" , PrimitiveFun (\(BasicT (Integer x)) -> boolean $ odd x))
  , (var "gcd", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `gcd` y)))
  , (var "lcm", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> BasicT $ Integer $ x `lcm` y)))
  -- Float  
  , (var "(+.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x + y)))
  , (var "(-.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x - y)))
  , (var "(*.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x * y)))
  , (var "(/.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x / y)))
  , (var "(>.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> boolean $ x > y)))
  , (var "(<.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> boolean $ x < y)))
  , (var "(<=.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> boolean $ x <= y)))
  , (var "(>=.)", PrimitiveFun (\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> boolean $ x >= y)))
  , (var "absF", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ abs x))
  , (var "negateF", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ negate x))
  , (var "minF", PrimitiveFun(\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x `min` y)))
  , (var "maxF", PrimitiveFun(\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x `max` y)))
  , (var "truncate", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Integer $ truncate x))
  , (var "round", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Integer $ round x))
  , (var "ceiling", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Integer $ ceiling x))
  , (var "floor", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Integer $ floor x))
  , (var "recip" , PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ recip x))
  , (var "pi", BasicT $ Float pi )
  , (var "exp", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ exp x))
  , (var "log", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ log x))
  , (var "sqrt", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ sqrt x))
  , (var "(**)", PrimitiveFun(\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ x ** y)))
  , (var "logBase", PrimitiveFun(\(BasicT (Float x)) -> PrimitiveFun (\(BasicT (Float y)) -> BasicT $ Float $ logBase x y)))
  , (var "sin", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ sin x))
  , (var "cos", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ cos x))
  , (var "tan", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ tan x))
  , (var "asin", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ asin x))
  , (var "acos", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ acos x))
  , (var "atan", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ atan x))
  , (var "sinh", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ sinh x))
  , (var "cosh", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ cosh x))
  , (var "tanh", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ tanh x))
  , (var "log1p", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ log1p x))
  , (var "expm1", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ expm1 x))
  , (var "log1pexp", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ log1pexp x))
  , (var "log1mexp", PrimitiveFun (\(BasicT (Float x)) -> BasicT $ Float $ log1mexp x))
  , (var "fromInteger", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Float $ Prelude.fromInteger (toInteger x)))
  -- Booleans
  , (var "(&&)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) && read (show y))))
  , (var "(||)", PrimitiveFun (\(Cons x _) -> PrimitiveFun (\(Cons y _) -> boolean $ read (show x) || read (show y))))
  , (var "(==)", PrimitiveFun (\(BasicT x) -> PrimitiveFun (\(BasicT y) -> boolean $ x == y)))
  , (var "(/=)", PrimitiveFun (\(BasicT x) -> PrimitiveFun (\(BasicT y) -> boolean $ x /= y)))

  , (var "(<)" , PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> boolean $ x <  y)))
  , (var "(>)" , PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> boolean $ x >  y)))
  , (var "(<=)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> boolean $ x <= y)))
  , (var "(>=)", PrimitiveFun (\(BasicT (Integer x)) -> PrimitiveFun (\(BasicT (Integer y)) -> boolean $ x >= y)))
  -- Chars
  , (var "chr", PrimitiveFun (\(BasicT (Integer x)) -> BasicT $ Character $ chr x))
  , (var "ord", PrimitiveFun (\(BasicT (Character x)) -> BasicT $ Integer $ ord x))
  -- Strings
  
--  , (var "(++)", PrimitiveFun (\(BasicT (String s1)) -> PrimitiveFun (\(BasicT (String s2)) -> BasicT $ String $ s1 ++ s2)))
  , (var "(++)", PrimitiveFun (\x -> PrimitiveFun (\y -> case x of
                    (BasicT (String s1)) -> let (BasicT (String s2)) = y in BasicT $ String $ s1 ++ s2
                    (Cons v xs)
                      | v == mkCons defaultSpan -> -- let (Cons _ ys) = y in
                          Cons v (xs ++ [[y]]) -- [x ++ y])
                      | v == mkNil defaultSpan -> let (Cons v ys) = y in Cons v (xs ++ [[y]])
                    e -> error $ show e
                                                  )))
  -- Show
  , (var "show", PrimitiveFun (BasicT . String . show))
  -- Read
  , (var "readBool", PrimitiveFun (\(BasicT (String s)) -> boolean (read s)))
  , (var "readInt" , PrimitiveFun (\(BasicT (String s)) -> BasicT $ Integer (read s)))
  , (var "readChar", PrimitiveFun (\(BasicT (String (c : _))) -> BasicT $ Character c))
--  Print to stdout
  , (var "__putStrOut", PrimitiveFun (\v -> IOValue $ putStr (show v) $> BasicT Unit))
--  Print to stderr
  , (var "__putStrErr", PrimitiveFun (\v -> IOValue $ hPutStr stderr (show v) $> BasicT Unit))
--  Read from stdin
  , (var "__getChar", PrimitiveFun (\_ -> IOValue $ getChar <&> (BasicT . Character)))
  , (var "__getLine", PrimitiveFun (\_ -> IOValue $ getLine <&> (BasicT . String)))
  -- Files
  , (var "__openFile",
      PrimitiveFun (\(BasicT (String s)) ->
      PrimitiveFun (\(Cons (Variable _ mode) _) -> IOValue $
        case mode of
          "ReadMode"   -> openFile s ReadMode   <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
          "WriteMode"  -> openFile s WriteMode  <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
          "AppendMode" -> openFile s AppendMode <&> Cons (var "FileHandle") . (: []) . (: []) . Handle
    )))
    
  , (var "__putFileStr",
      PrimitiveFun (\(Cons _ [[Handle fh]]) -> PrimitiveFun (\(BasicT (String s)) ->
        IOValue $ hPutStr fh s $> BasicT Unit
    )))
  , (var "__readFileChar", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetChar fh <&> BasicT . Character))
  , (var "__readFileLine", PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hGetLine fh <&> BasicT . String))
  , (var "__isEOF"       , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hIsEOF fh <&> boolean))
  , (var "__closeFile"   , PrimitiveFun (\(Cons _ [[Handle fh]]) -> IOValue $ hClose fh $> BasicT Unit))
  -- Id  
  , (var "id", PrimitiveFun id)
 -- --  Undefined
 -- , (var "undefined", PrimitiveFun undefined)
 -- --  Error
 --  , (var "error", PrimitiveFun (\(BasicT (String e)) ->
 --         unsafePerformIO $ die $ showErrors "" Map.empty [] (ErrorFunction s e)))
  ]
 where
  var :: String -> Variable
  var = mkVar defaultSpan
  boolean :: Bool -> Value
  boolean b = Cons (mkVar defaultSpan (show b)) []


