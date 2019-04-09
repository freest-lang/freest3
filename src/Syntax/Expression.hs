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

module Syntax.Expression
( Expression(..)
, FieldMap
) where

import           Parse.Lexer (Position, Pos, position, showPos)
import           Syntax.Types (Type)
import           Syntax.Kinds (Multiplicity(..))
import           Syntax.Bind (PVar, PBind)
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | ProgVar Pos PVar
  -- Abstraction intro and elim
  | Lambda Pos Multiplicity PBind Type Expression
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos {- Multiplicity -} Expression Expression
  | BinLet Pos PBind PBind Expression Expression
  -- Datatype intro and elim
  | Case Pos Expression FieldMap
  -- Type application
  | TypeApp Pos PVar [Type]
  -- Boolean elim
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos PBind Expression Expression -- TODO: Derived; eliminate?
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression
  | Receive Pos Expression
  | Select Pos PVar Expression
  | Match Pos Expression FieldMap
  deriving (Eq, Ord)

type FieldMap = Map.Map PBind Expression
-- type ExpMap  = Map.Map PBind ([PBind], Expression)

instance Show Expression where
  show e = showAux e 2

showAux :: Expression -> Int -> String
showAux _ 0 = " ... "
-- Basic values
showAux (Unit _) _  = " ()"
showAux (Integer _ i) _ = show i
showAux (Character _ c) _ = show c
showAux (Boolean _ b) _  = show b
-- Variable
showAux (ProgVar _ x) _  = show x
  -- Abstraction intro and elim
showAux (Lambda _ Un b t e) i = "(" ++ "\\" ++ show b ++ " : " ++ show t ++ " -> " ++ (showAux e (i-1)) ++ ")"
showAux (Lambda _ Lin b t e) i = "(" ++ "\\" ++ show b ++ " : " ++ show t ++ "-o" ++ (showAux e (i-1)) ++ ")"
showAux (App _ e1 e2) i = (showAux e1 (i-1)) ++ (showAux e2 (i-1))
  -- Pair intro and elim
showAux (Pair _ e1 e2) i = " (" ++ (showAux e1 (i-1)) ++ ", " ++ (showAux e1 (i-1)) ++ ")"
showAux (BinLet _ b1 b2 e1 e2) i = " let " ++ show b1 ++ ", " ++ show b2 ++ " = " ++ (showAux e1 (i-1)) ++ " in " ++ (showAux e2 (i-1))
  -- Datatype elim
showAux (Case _ e m) i = " case " ++ (showAux e (i-1)) ++ " of " ++ (showMap m (i-1))
  -- Type application
showAux (TypeApp _ x [t]) _ = x ++ show t
  -- Boolean elim
showAux (Conditional _ e e1 e2) i = " if " ++ show e ++ " then " ++ (showAux e1 (i-1)) ++ " else " ++ (showAux e2 (i-1))
  -- Let
showAux (UnLet _ b1 e1 e2) i = " let " ++ show b1 ++ " = " ++ (showAux e1 i) ++ " in " ++ (showAux e2 (i-1))
-- Fork
showAux (Fork _ e) i = " fork " ++ (showAux e (i-1))
  -- Session types
showAux (New _ t) _ = " new " ++ show t
showAux (Send _ e) _ = " send " ++ show e
showAux (Receive _ e) _ = " receive " ++ show e
showAux (Select _ l e) i = " select " ++ show l ++ (showAux e (i-1))
showAux (Match _ e m) i = " match " ++ show e ++ " with " ++ (showMap m (i-1))

showMap :: FieldMap -> Int -> String
showMap m i = concat $ intersperse ", " (map showAssoc (Map.toList m))
  where showAssoc (b, v) = (show b) ++ " " ++ (showAux v (i-1))

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
