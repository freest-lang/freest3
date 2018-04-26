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
  , Pos
  , HaskellCode
  , translate
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

type Pos = (Int, Int)
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
  | Variable Pos TermVar
  | UnLet Pos TermVar Expression Expression
  -- Aplication
  | App Pos Expression Expression
  | TypeApp Pos Expression Type
  -- Conditional
  | Conditional Pos Expression Expression Expression
  -- Pairs
  | Pair Pos Expression Expression
  | Let Pos TermVar TermVar Expression Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression Expression
  | Receive Pos Expression 
  | Select Pos TermVar Expression
  | Match Pos Expression MatchMap  
  -- Branch - overloaded with Case
  -- Fork
  | Fork Pos Expression
  -- Datatypes
  | Constructor Pos TermVar
  | Case Pos Expression CaseMap
--  deriving Eq
--   deriving Show
-- ("parseCase",([],Case (App (App (Variable "(+)") (Integer 2)) (Integer 2))
--  (fromList [("C",(["a"],Integer 23)),("D",(["a"],Integer 24)),("E",(["a"],Integer 25))])))

instance Show Expression where
  show  Unit               = "()"
  show (Integer i)         = show i
  show (Character c)       = show c
  show (Boolean b)         = show b
  show (Variable _ v)      = v
  show (UnLet _ tv e1 e2)    = showUnLet tv e1 e2
  show (App _ e1 e2) = showApp e1 e2
  show (TypeApp _ e1 t) = show e1 -- TODO: proper show
  show (Conditional _ e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Pair _ e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Let _ tv1 tv2 e1 e2) = showLet tv1 tv2 e1 e2
  -- TODO...
  show (New _ _) = "new" -- ++ show t
  show (Send _ e1 e2) = showSend e1 e2
  show (Receive _ e) = "receive " ++ show e
  show (Select _ tv e) = "Select " ++ tv ++ " " ++ show e
--  show (Match e1 (Map.Map termVar (typeVar, es))) =
  show (Fork _ e) = showFork e
  show (Constructor _ tv) = tv
  show (Case _ e cm) = "case " ++ show e ++ " of\n  " ++ (showCaseMap cm)
  show (Match _ e cm) = "case " ++ show e ++ " of\n  " ++ (showMatchMap cm)

showApp :: Expression -> Expression -> String
showApp (App _ (Variable _ ('(':op:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ show e3 ++ ")"
showApp (App _ (Variable _ ('(':op:op2:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ [op2] ++ show e3 ++ ")"
showApp (Variable _ "negate") e2                = "-" ++ show e2
showApp (App _ (Variable _ "div") e2) e3  = show e2 ++ " `div` " ++ show e3
showApp (App _ (Variable _ "rem") e2) e3  = show e2 ++ " `rem` " ++ show e3
showApp e1 e2                             = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

showCaseMap :: CaseMap -> String
showCaseMap = Map.foldlWithKey (\acc tv (param, e) -> acc ++ tv ++ " " ++
                                 param ++ "-> " ++ show e ++ "\n  ") ""

showMatchMap :: MatchMap -> String
showMatchMap = Map.foldlWithKey (\acc tv (params, e) -> acc ++ tv ++ " " ++
                                 showParams params ++ "-> " ++ show e ++ "\n  ") ""  

-- TODO use on match
showParams :: Params -> String
showParams as
  | null as = ""
  | otherwise = " " ++ (intercalate " " as)

showLet :: TermVar -> TermVar -> Expression -> Expression -> String
showLet tv1 tv2 e1 e2 = "let (" ++ tv1 ++ ", " ++ tv2 ++ ") = " ++ show e1 ++  " in\n  " ++ show e2

showUnLet tv e1 e2 = "let " ++ tv ++ " = " ++ show e1 ++ "\n  " ++ show e2

showFork e = "fork " ++ show e
showSend e1 e2 = "send " ++ show e1 ++ " " ++ show e2


-- Translate from X code to Haskell code

type HaskellCode = String

translate :: Expression -> (HaskellCode, Bool)
translate Unit = ("()", False)
translate (Integer i) = (show i, False)
translate (Character c) = (show c, False)
translate (Boolean b) =  (show b, False)
translate (Variable _ x) =  (x, False)
translate (Constructor _ c) =  (c, False)
translate (Pair _ e1 e2) =
  ("(" ++ fst(translate e1) ++ ", " ++ fst (translate e2) ++ ")", False)

translate (UnLet _ x e1 e2) =
  let (h1,b1) = translate e1
      (h2,b2) = translate e2 in
  if b1 then
    (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, b1 || b2) 
  else
    ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)

translate (Let _ x y e1 e2) =
  let (h1,b1) = translate e1
      (h2,b2) = translate e2 in
  if b1 then
    (h1 ++ " >>= \\(" ++ x ++ "," ++ y ++ ")" ++ " -> " ++ h2, b1 || b2) 
  else
    ("let (" ++ x ++ "," ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)

translate (Conditional _ c e1 e2) =
  let (h1, b1) = translate e1
      (h2, b2) = translate e2 in
  ("if " ++ (fst (translate c)) ++ " then " ++ h1 ++ " else " ++ h2, b1 || b2)
      
translate (App _ e1 e2) =
  let (h1,b1) = translate e1
      (h2,b2) = translate e2 in
  if b2 then
    -- freshVar
    (h2 ++ " >>= \\ " ++ "x1' " ++ " -> " ++ h1, b1 || b2)
  else
    ("(" ++ h1 ++ " " ++  h2 ++ ")", False)

translate (Send _ e1 e2) =
  let (h1, b1) = translate e1
      (h2, b2) = translate e2 in
  if b2 then
    -- freshVar
    (h2 ++ " >>= \\" ++  "x2'" ++ " -> " ++ "send " ++ h1, True)
  else
    ("(send (" ++ h1 ++ ")" ++ "(" ++  h2 ++ "))", True)

translate (Receive _ e) =
  let (h1, b1) = translate e in
  if b1 then
    -- freshVar
    ("\\" ++  "x3'" ++ " -> " ++ "receive " ++ h1, True)
  else
    ("receive " ++ h1 , True)

translate (New _ _) =
--  let (h,_) = translate e in
  ("new ", True)

translate (Fork _ e) =
  let (h,_) = translate e in
  ("fork " ++ h, True)

translate (Match _ e m) =
  let (h1,_) = translate e
      h2 = translateMatchMap m in
  ("case " ++ h1 ++ " of " ++ h2, False)

translate (Case _ e m) =
  let (h1, _) = translate e
      (h2, params) = translateCaseMap m in 
 --   if b1 then
      -- fresh var
      ("receive " ++ h1 ++ " >>= \\(" ++ "fRESH3" ++  ", " ++ (head params) ++ ") -> case "
       ++ "fRESH3" ++ " of " ++ h2, False) 
    -- else
    --   ("b1 " ++ show b1 ++ " b2 " ++ show b2  ++ "\n" ++ "case " ++ h1 ++ " of " ++ h2, b2)

translate (Select _ l e) =
  let (h, _) = translate e in
    ("send \"" ++ l ++ "\" " ++ h, True)
  

translate e = (show e, True)


-- typeapp
-- select
-- case

translateMatchMap = Map.foldlWithKey translateMatchMap' "" 
  where
    translateMatchMap' acc v (params, e) =
      let (h, _) = translate e in
        acc ++ "\n    " ++ v ++ showParams params ++ " -> " ++ h ++ " "


translateCaseMap = Map.foldlWithKey translateCaseMap' ("", [])
  where
    translateCaseMap' acc v (param, e) =
      let (h, _) = translate e in
        (fst acc ++ "\n    \"" ++ v ++ "\" " ++ " -> "
         ++ h ++ " >>= \\" ++ "fRESH5" ++ " -> return ()", snd acc ++ [param])
    -- translateCaseExpr h =
    --   let (x,y) = splitAt (last (findIndices (`elem` (">=" :: String)) h) + 1) h in
    --     x ++ " return" ++ y
        
