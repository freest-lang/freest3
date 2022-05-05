module Interpreter.Value
  ( Value(..)
  , Ctx
  , Channel -- Do we need this one?
  , ChannelEnd
  )
where

import qualified Control.Concurrent.Chan       as C
import qualified Data.Map.Strict               as Map
import           Parse.Unparser                 ( )
import           Syntax.Base
import qualified Syntax.Expression             as E

data Value =
    Unit
  | Integer Int
  | Boolean Bool
  | Character Char
  | Label String -- to be sent on channels
  | String String
  | Cons Variable [[Value]] -- TODO: Think how to do this in other way
  | Pair Value Value
  | Closure Variable E.Exp Ctx
  | TypeAbs E.Exp Ctx
  | PrimitiveFun (Value -> Value)
  | Chan ChannelEnd
  | Fork
  | IOValue (IO Value)

type Ctx = Map.Map Variable Value

type ChannelEnd = (C.Chan Value, C.Chan Value)
type Channel = (ChannelEnd, ChannelEnd)

instance Show Value where
  show Unit           = "()"
  show (Integer   i)  = show i
  show (Boolean   b)  = show b
  show (Character c)  = show c
  show (String    s)  = s
  show (Label     s)  = s
  show (Pair v1 v2 )  = "(" ++ show v1 ++ ", " ++ showTuple v2 ++ ")"
  show (Cons c  xs )
    | show c ==  "[]"  = "[]"
    | show c == "(::)" = let ([y]:ys) = xs in "[" ++ show y ++ showNativeList ys ++ "]"
    | otherwise        = showCons c xs
  show Closure{}      = "<fun>"
  show TypeAbs{}      = "<fun>"
  show PrimitiveFun{} = "<fun>"
  show Chan{}         = "Skip" -- TODO: change this
  show Fork           = "fork"
  show IOValue{}      = "<IOValue>"

showTuple :: Value -> String
showTuple (Pair v1 v2) = show v1 ++ ", " ++ showTuple v2
showTuple v            = show v

showCons :: Variable -> [[Value]] -> String
showCons x [] = show x
showCons x xs = show x ++ " " ++ unwords (map showConstrList xs)
 where
  showConstrList :: [Value] -> String
  showConstrList = unwords . map showC

  showC :: Value -> String
  showC c@(Cons _ []) = show c
  showC c@(Cons c' _) | show c' == "(::)" = show c
  showC c@Cons{}      = "(" ++ show c ++ ")"
  showC v             = show v

showNativeList :: [[Value]] -> String
showNativeList [[Cons _ []]]           = ""
showNativeList ([(Cons _ ([y]:ys))]:_) = "," ++ show y ++ showNativeList ys
-- showNativeList xs = show xs

instance Position Value where
  pos _ = defaultPos
