module Terms.Terms
  ( Expression(..)
  , ExpEnv
  , VarEnv
  , TypeEnv
  , ConstructorEnv
  , TermVar
  , Params
  , CaseMap
  , MatchMap
  ) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types
import           Data.List

type TermVar = String

type Params = [TermVar]

type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Params, Expression)

type TypeEnv = Map.Map TypeVar Type
-- type TypeEnv = Map.Map TypeVar (Kind, Type)

type ConstructorEnv = Map.Map TypeVar Type

data TypeVarBind = TypeVar Kind
data TypeScheme = Functional Type | Scheme TypeVarBind TypeScheme

-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

type CaseMap = Map.Map TermVar (TermVar, Expression)
type MatchMap = Map.Map TermVar (Params, Expression)

data Expression
  -- Basic expressions
  = Unit
  | Integer Int
  | Character Char
  | Boolean Bool
  -- Variables
  | Variable TermVar
  | UnLet TermVar Expression Expression
  -- Aplication
  | App Expression Expression
  | TypeApp Expression Type
  -- Conditional
  | Conditional Expression Expression Expression
  -- Pairs
  | Pair Expression Expression -- TODO: Express as application
  | Let TermVar TermVar Expression Expression
  -- Session types
  | New Type
  | Send Expression Expression -- TODO: Express as application
  | Receive Expression -- TODO: Express as application
  | Select TermVar Expression
  | Match Expression MatchMap  
  -- Branch - overloaded with Case
  -- Fork
  | Fork Expression -- TODO: Express as application
  -- Datatypes
  | Constructor TermVar
  | Case Expression CaseMap
--  deriving Eq
--   deriving Show
-- ("parseCase",([],Case (App (App (Variable "(+)") (Integer 2)) (Integer 2))
--  (fromList [("C",(["a"],Integer 23)),("D",(["a"],Integer 24)),("E",(["a"],Integer 25))])))

instance Show Expression where
  show  Unit               = "()"
  show (Integer i)         = show i
  show (Character c)       = show c
  show (Boolean b)         = show b
  show (Variable v)        = v
  show (UnLet tv e1 e2)        = "let " ++ tv ++ " = " ++ show e1 ++ " in " ++ show e2
  show (App e1 e2) = showApp e1 e2
  show (TypeApp e1 t) = show e1 -- TODO: proper show
  show (Conditional e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Pair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Let tv1 tv2 e1 e2) = showLet tv1 tv2 e1 e2
  -- TODO...
  show (New t) = "New " ++ show t
  show (Send e1 e2) = "Send " ++ show e1 ++ " " ++ show e2
  show (Receive e1) = "Receive " ++ show e1
  show (Select tv e1) = "Select " ++ tv ++ " " ++ show e1 
--  show (Match e1 (Map.Map termVar (typeVar, es))) =
  show (Fork e1) = "Fork " ++ show e1
  show (Constructor tv) = tv
  show (Case e1 cm) = "case " ++ show e1 ++ " of\n  " ++ (showCaseMap cm)
  show (Match e1 cm) = "case " ++ show e1 ++ " of\n  " ++ (showMatchMap cm)

showApp :: Expression -> Expression -> String
showApp (App (Variable ('(':op:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ show e3 ++ ")"
showApp (App (Variable ('(':op:op2:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ [op2] ++ show e3 ++ ")"
-- showApp (App (Variable "(+)") e2) e3  = "(" ++ show e2 ++ " + " ++ show e3 ++ ")"
-- showApp (App (Variable "(-)") e2) e3  = "(" ++ show e2 ++ " - " ++ show e3 ++ ")"
-- showApp (App (Variable "(/)") e2) e3  = "(" ++ show e2 ++ " / " ++ show e3 ++ ")"
-- showApp (App (Variable "(*)") e2) e3  = "(" ++ show e2 ++ " * " ++ show e3 ++ ")"
-- showApp (App (Variable "(==)") e2) e3 = "(" ++ show e2 ++ " == " ++ show e3 ++ ")"
-- showApp (App (Variable "(>=)") e2) e3 = "(" ++ show e2 ++ " == " ++ show e3 ++ ")"
showApp (Variable "negate") e2                = "-" ++ show e2
showApp (App (Variable "div") e2) e3  = show e2 ++ " `div` " ++ show e3
showApp (App (Variable "rem") e2) e3  = show e2 ++ " `rem` " ++ show e3
-- TODO others
-- showApp (App e1 e2) e3                = show e2 ++ " " ++ show e1  ++ " " ++ show e3
showApp e1 e2                                 = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

showLet :: TermVar -> TermVar -> Expression -> Expression -> String
showLet tv1 tv2 e1 e2 = "let (" ++ tv1 ++ ", " ++ tv2 ++ ") = " ++ show e1 ++  " in " ++ show e2

-- type CaseMap = Map.Map TermVar (Params, Expression)
showCaseMap :: CaseMap -> String
showCaseMap = Map.foldlWithKey (\acc tv (param, e) -> acc ++ tv ++ " " ++
                                 param ++ "-> " ++ show e ++ "\n  ") ""
-- TODO: review
showMatchMap :: MatchMap -> String
showMatchMap = Map.foldlWithKey (\acc tv (params, e) -> acc ++ tv ++ " " ++
                                 showParams params ++ "-> " ++ show e ++ "\n  ") ""  

-- TODO use on match
showParams :: Params -> String
showParams as
  | null as = ""
  | otherwise = " " ++ (intercalate " " as)

