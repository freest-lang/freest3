{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Expressions
( Expression(..)
, FieldMap
, ExpEnv
) where

import           Syntax.Types (Type)
import           Syntax.ProgramVariables
import           Syntax.Base
import           Data.List (intersperse, intercalate)
import qualified Data.Map.Strict as Map

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | ProgVar Pos ProgVar
  -- Abstraction intro and elim
  | Lambda Pos Multiplicity ProgVar Type Expression
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos {- Multiplicity -} Expression Expression
  | BinLet Pos ProgVar ProgVar Expression Expression
  -- Datatype elim
  | Case Pos Expression FieldMap
  -- Type application
  | TypeApp Pos ProgVar [Type]
  -- Boolean elim
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos ProgVar Expression Expression -- TODO: Derived; eliminate?
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression
  | Receive Pos Expression
  | Select Pos ProgVar Expression
  | Match Pos Expression FieldMap
--  deriving (Eq, Ord)

type FieldMap  = Map.Map ProgVar ([ProgVar], Expression)

-- The definitions of the named functions in a program
type ExpEnv = Map.Map ProgVar Expression

instance Show Expression where
  show e = showExp e 4

showExp :: Expression -> Int -> String
  -- Basic values
showExp (Unit _) _  = "()"
showExp (Integer _ i) _ = show i
showExp (Character _ c) _ = show c
showExp (Boolean _ b) _  = show b
  -- Variable
showExp (ProgVar _ x) _  = show x
  -- Depth reached
showExp _ 0 = " .. "
  -- Abstraction intro and elim
showExp (Lambda _ m b t e) i = "(\\" ++ show b ++ " : " ++ show t ++ showArrow m ++ (showExp e (i-1)) ++ ")"
showExp (App _ e1 e2) i = showExp e1 (i-1) ++ " " ++ showExp e2 (i-1)
  -- Pair intro and elim
showExp (Pair _ e1 e2) i = " (" ++ (showExp e1 (i-1)) ++ ", " ++ (showExp e1 (i-1)) ++ ")"
showExp (BinLet _ b1 b2 e1 e2) i = "let " ++ show b1 ++ ", " ++ show b2 ++ " = " ++ showExp e1 (i-1) ++ " in " ++ showExp e2 (i-1)
  -- Datatype elim
showExp (Case _ e m) i = "case " ++ showExp e (i-1) ++ " of {" ++ showMap m (i-1) ++ "}"
  -- Type application
showExp (TypeApp _ x ts) _ = show x ++ " [" ++ (intercalate " " (map show ts)) ++ "]"
  -- Boolean elim
showExp (Conditional _ e e1 e2) i = "if " ++ show e ++ " then " ++ showExp e1 (i-1) ++ " else " ++ showExp e2 (i-1)
  -- Let
showExp (UnLet _ b1 e1 e2) i = "let " ++ show b1 ++ " = " ++ showExp e1 (i-1) ++ " in " ++ showExp e2 (i-1)
  -- Fork
showExp (Fork _ e) i = " fork " ++ (showExp e (i-1))
  -- Session types
showExp (New _ t) _ = "new " ++ show t
showExp (Send _ e) _ = "send " ++ show e
showExp (Receive _ e) _ = "receive " ++ show e
showExp (Select _ l e) i = "select " ++ show l ++ showExp e (i-1)
showExp (Match _ e m) i = "match " ++ show e ++ " with {" ++ showMap m i ++ "}"

showMap :: FieldMap -> Int -> String
showMap m i = concat $ intersperse "; " (map showAssoc (Map.toList m))
  where showAssoc (b, (a,v)) = show b ++ " " ++ intercalate " " (map show a) ++ " -> " ++  (showExp v (i-1))

instance Position Expression where
  position (Unit p)              = p
  position (Integer p _)         = p
  position (Character p _)       = p
  position (Boolean p _)         = p
  position (ProgVar p _)         = p
  position (Lambda p _ _ _ _)    = p
  position (UnLet p _ _ _)       = p
  position (App p _ _)           = p
  position (TypeApp p _ _)       = p
  position (Conditional p _ _ _) = p
  position (Pair p _ _)          = p
  position (BinLet p _ _ _ _)    = p
  position (New p _)             = p
  position (Send p _)            = p
  position (Receive p _ )        = p
  position (Select p _ _)        = p
  position (Match p _ _)         = p
  position (Fork p _)            = p
  position (Case p _ _)          = p
