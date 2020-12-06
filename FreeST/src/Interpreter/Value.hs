module Interpreter.Value
( Value(..)
, Ctx
, Channel -- Do we need this one?
, ChannelEnd
)
where

import qualified Control.Concurrent.Chan as C
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Syntax.Expression as E
import           Syntax.ProgramVariable
import           Parse.Unparser

data Value =
    Unit
  | Integer Int
  | Boolean Bool
  | Character Char
  | Cons ProgVar [[Value]] -- TODO: Think how to do this in other way
  | Pair Value Value
  | Closure ProgVar E.Exp Ctx
  | PrimitiveFun (Value -> Value)
  | Label String -- to be sent over channels
  | Chan ChannelEnd
  | Fork
  | IOValue (IO Value)
--  | Send ChannelEnd
--  | Receive

type Ctx = Map.Map ProgVar Value

type ChannelEnd = (C.Chan Value, C.Chan Value)
type Channel    = (ChannelEnd, ChannelEnd)


instance Show Value where
  show Unit          = "()"
  show (Integer i)   = show i
  show (Boolean b)   = show b
  show (Character c) = show c
  show (Label s)     = s
  show (Pair v1 v2)  = "(" ++ show v1 ++ ", " ++ showNTupleValue v2 ++ ")"
  show c@Cons{}  = showCons c
  show (Chan _)      = "Skip" -- TODO: change this
  show (Closure x e _)  = show x ++ " " ++ show e-- TODO: change this

showNTupleValue :: Value -> String
showNTupleValue (Pair v1 v2) = show v1 ++ ", " ++ showNTupleValue v2
showNTupleValue v            = show v

showCons :: Value -> String
showCons (Cons x []) = show x
showCons (Cons x xs) = show x ++ " " ++ unwords (map showConstrList xs)
 where
   showConstrList :: [Value] -> String
   showConstrList = unwords . map showC

   showC :: Value -> String
   showC c@(Cons _ []) = show c
   showC c@Cons{} = "(" ++ show c ++ ")"
   showC v = show v
