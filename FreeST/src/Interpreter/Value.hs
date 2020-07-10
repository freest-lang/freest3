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
import qualified Syntax.Expressions as E
import           Syntax.ProgramVariables
import           Syntax.Show()
import           Syntax.Base

data Value =
    Unit
  | Integer Int
  | Boolean Bool
  | Character Char  
  | Cons ProgVar [[Value]] -- TODO: Think how to do this in other way
  | Pair Value Value
  | Closure ProgVar E.Expression Ctx
  | PrimitiveFun (Value -> Value)
  | Label String -- to be sent over channels
  | Chan ChannelEnd
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
  show (Pair v1 v2)  = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
  show c@(Cons _ _)  = showCons c
  show (Chan _)      = "Skip" -- TODO: change this
  show (Closure x e _)  = show x ++ " " ++ show e-- TODO: change this


showCons :: Value -> String
showCons (Cons x [])
  | x == mkVar defaultPos "#Nil"  = "[]"
  | otherwise                     = show x
showCons c@(Cons x xs)
  | x == mkVar defaultPos "#Cons" = showBuiltinList c
  | otherwise = show x ++ " " ++ (intercalate " " (map showConstrList xs))
 where
   showConstrList :: [Value] -> String
   showConstrList xs = intercalate " " (map showC xs)

   showC :: Value -> String
   showC c@(Cons _ []) = show c
   showC c@(Cons _ _) = "(" ++ show c ++ ")"
   showC v = show v

showBuiltinList :: Value -> String
showBuiltinList (Cons x xs) = '[' : (init (intercalate "," (map showList' xs)) ++ "]")
  where
    showList' :: [Value] -> String
    showList' xs = intercalate "," $ map showElem xs

    showElem :: Value -> String
    showElem (Cons x xs)
      | x == mkVar defaultPos "#Nil" = ""
      | otherwise                    =  intercalate "," (map showList' xs)
    showElem v = show v

-- "#Cons 1 (#Cons 2 (#Cons 3 (#Cons 4 (#Cons 5 #Nil))))"
