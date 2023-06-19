module Interpreter.Value
  ( Value(..)
  , Basic(..)
  , Ctx
  , Channel -- Do we need this one?
  , ChannelEnd
  )
where

import           Parse.Unparser ( )
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName

import qualified Control.Concurrent.Chan as C
import qualified Data.Map.Strict as Map


import           System.IO                      ( Handle )

data Value =
    BasicT Basic
  | Label String -- to be sent on channels
  | Cons Variable [[Value]] -- TODO: Think how to do this in other way
  | Pair Value Value
  | Closure Variable Variable E.Exp Ctx -- The first variable is just the function name
  | TypeAbs E.Exp Ctx
  | PrimitiveFun (Value -> Value)
  | Chan ChannelEnd
  | Fork
  | IOValue (IO Value)
  | Handle Handle

data Basic =
    Unit
  | Integer Int
  | Character Char
  | String String
  | Float Double
  deriving Eq

instance Show Basic where
  show Unit           = "()"
  show (Integer   i)  = show i
  show (Character c)  = show c
  show (String    s)  = s
  show (Float     f)  = show f

type Ctx = Map.Map Variable Value

type ChannelEnd = (C.Chan Value, C.Chan Value)
type Channel = (ChannelEnd, ChannelEnd)

instance Show Value where
  show (BasicT b) = show b
  show (Label     s)  = s
  show (Pair v1 v2 )  = "(" ++ show v1 ++ ", " ++ showTuple v2 ++ ")"
  show (Cons c  xs )
--   = "Cons " ++ show c ++ " " ++ show xs
    | c == mkCons defaultSpan = let ([y]:ys) = xs in "[" ++ show y ++ showNativeList ys ++ "]"
    | otherwise               = showCons c xs
  show Closure{}      = "<fun>"
  show TypeAbs{}      = "<fun>"
  show PrimitiveFun{} = "<fun>"
  show Chan{}         = "Skip" -- TODO: change this
  show Fork           = "fork"
  show IOValue{}      = "<IOValue>"
  show (Handle h)     = show h 

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
  showC c@(Cons c' _) | c' == mkCons defaultSpan = show c
  showC c@Cons{}      = "(" ++ show c ++ ")"
  showC v             = show v


showNativeList :: [[Value]] -> String
showNativeList []           = ""
showNativeList ([Cons _ ([y]:ys)]:zs) = "," ++ show y ++ showNativeList ys ++ showNativeList zs
showNativeList _ = ""

instance Located Value where
  getSpan _ = defaultSpan


