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
import           Control.Monad.State


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
  | BinLet Pos TermVar TermVar Expression Expression
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
  show (TypeApp _ e1 t) = show e1
  show (Conditional _ e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Pair _ e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (BinLet _ tv1 tv2 e1 e2) = showBinLet tv1 tv2 e1 e2
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

showBinLet :: TermVar -> TermVar -> Expression -> Expression -> String
showBinLet tv1 tv2 e1 e2 = "let (" ++ tv1 ++ ", " ++ tv2 ++ ") = " ++ show e1 ++  " in\n  " ++ show e2

showUnLet tv e1 e2 = "let " ++ tv ++ " = " ++ show e1 ++ "\n  " ++ show e2

showFork e = "fork " ++ show e
showSend e1 e2 = "send " ++ show e1 ++ " " ++ show e2


-- Translate from X code to Haskell code

type HaskellCode = String
type TranslateMonad = State Int

-- nextState :: Int->Int
-- nextState x = 1+x

-- getNextFreshVar :: TranslateMonad String
-- getNextFreshVar  = state  (\st -> let st' = nextState(st) in ("_Var" ++ show st',st') )

translate :: Expression -> TranslateMonad (HaskellCode, Bool)
translate Unit = return ("()", False)
translate (Integer i) = return (show i, False)
translate (Character c) = return (show c, False)
translate (Boolean b) =  return (show b, False)
translate (Variable _ x) =  return (x, False)
translate (Constructor _ c) =  return (c, False)
translate (Pair _ e1 e2) = do
  h1 <- translate e1
  h2 <- translate e2
  return ("(" ++ fst h1 ++ ", " ++ fst h2 ++ ")", False)

translate (UnLet _ x e1 e2) = do 
  (h1,b1) <- translate e1
  (h2,b2) <- translate e2
  if b1 then
    return (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, b1 || b2) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)

translate (BinLet _ x y e1 e2) = do
  (h1,b1) <- translate e1
  (h2,b2) <- translate e2
  if b1 then
    return (h1 ++ " >>= \\(" ++ x ++ "," ++ y ++ ")" ++ " -> " ++ h2, b1 || b2) 
  else
    return ("let (" ++ x ++ "," ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)

translate (Conditional _ c e1 e2) = do
  (c1, _) <- translate c
  (h1, b1) <- translate e1
  (h2, b2) <- translate e2
  return ("if " ++ c1 ++ " then " ++ h1 ++ " else " ++ h2, b1 || b2)
      
translate (App _ e1 e2) = do
  (h1,b1) <- translate e1
  (h2,b2) <- translate e2
  if b2 then
    do
      fresh <- get
      modify (+1)
      return (h2 ++ " >>= \\ " ++ "_Var" ++ show fresh ++ " -> " ++ h1, b1 || b2)
  else
    return ("(" ++ h1 ++ " " ++  h2 ++ ")", False)

translate (TypeApp _ e _) = translate e

translate (Send _ e1 e2) = do
  (h1, b1) <- translate e1
  (h2, b2) <- translate e2
  if b2 then
    -- freshVar
    do
      fresh <- get
      modify (+1)
      return (h2 ++ " >>= \\" ++  "_Var" ++ show fresh ++ " -> " ++ "send " ++ h1, True)
  else
    return ("(send (" ++ h1 ++ ")" ++ "(" ++  h2 ++ "))", True)

translate (Receive _ e) = do
  (h1, b1) <- translate e
  if b1 then
    do
      fresh <- get
      modify (+1)
      return ("\\" ++ "_Var" ++ show fresh ++ " -> " ++ "receive " ++ h1, True)
  else
    return ("receive " ++ h1 , True)

translate (New _ _) = do
--  let (h,_) = translate e in
  return ("new ", True)

translate (Fork _ e) = do
  (h,_) <- translate e
  return ("fork " ++ h, True)

translate (Match _ e m) = do
  (h1,_) <- translate e
  h2 <- translateMatchMap m
  return ("case " ++ h1 ++ " of " ++ h2, False)

translate (Case _ e m) = do
  (h1, _) <- translate e
  (h2, params) <- translateCaseMap m
  fresh <- get
  modify (+1)
  let fvar = "_Var" ++ show fresh in
    return ("receive " ++ h1 ++ " >>= \\(" ++ fvar ++  ", " ++ (head params) ++
          ") -> case " ++ fvar ++ " of " ++ h2, False) 

translate (Select _ l e) = do
  (h, _) <- translate e
  return ("send \"" ++ l ++ "\" " ++ h, True)
  

translateMatchMap :: MatchMap -> TranslateMonad String
translateMatchMap = Map.foldlWithKey translateMatchMap' (return "")
  where
    translateMatchMap' acc v (params, e) = do
      (h, _) <- translate e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showParams params ++ " -> " ++ h ++ " ")


translateCaseMap :: CaseMap -> TranslateMonad (String, [String])
translateCaseMap = Map.foldlWithKey translateCaseMap' (return ("", []))
  where
    translateCaseMap' acc v (param, e) = do
      (h, _) <- translate e
      acc' <- acc
      fresh <- get
      modify (+1) 
      return (fst acc' ++ "\n    \"" ++ v ++ "\" " ++ " -> "
         ++ h ++ " >>= \\" ++ "_Var" ++ show fresh ++ " -> return ()", snd acc' ++ [param])
