{- |
Module      :  Syntax.Show
Description :  The show module
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

Converting AST terms to strings.
-}

module Syntax.Show
  ( showChoiceView
  , showFieldMap
  )
where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Validation.Substitution       as Subs
                                                ( unfold ) -- no renaming
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intersperse
                                                , intercalate
                                                )
import           Data.Char                      ( isDigit )

-- Positions (Base)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c

-- Multiplicities (Base)

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

showArrow :: Multiplicity -> String
showArrow Lin = " -o "
showArrow Un  = " -> "

-- Program Variables. Note: show should be aligned with the creation
-- of new variables; see Syntax.ProgramVariables

instance Show ProgVar where
  show = showVar

-- Type Variables. Note: show should be aligned with the creation
-- of new variables; see Syntax.TypeVariables

instance Show TypeVar where
  show = showVar

showVar :: Variable v => v -> String
showVar v | isDigit (head s) = dropWhile (\x -> isDigit x || x == '#') s
          | otherwise        = s
  where s = intern v
-- showVar = intern -- for testing purposes

-- Kinds

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"
  show MessageK   = "M"

instance Show Kind where
  show (Kind _ p m) = show p ++ show m
  show (KindArrow _ k1 k2) = show k1 ++ " -> " ++ show k2

instance Show KindBind where
--  show (KindBind _ a k) = show a
  show (KindBind _ a k) = show a ++ ":" ++ show k

-- Types

instance Show Polarity where
  show In  = "?"
  show Out = "!"

showChoiceView :: Polarity -> String
showChoiceView In  = "&"
showChoiceView Out = "+"

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

instance Show Type where
--  show = showType 4
  show = showType 44 -- for testing purposes

showType :: Int -> Type -> String
  -- Non-recursive cases
showType _ (Basic _ b    ) = show b
showType _ (Skip _       ) = "Skip"
showType _ (TypeVar _ x  ) = show x
showType _ (Message _ p b) = show p ++ show b
showType _ (TypeName _ x ) = show x
  -- Depth reached
showType 0 _               = ".."
  -- Functional types
showType i (Fun _ m t u) =
  "(" ++ showType (i - 1) t ++ showArrow m ++ showType (i - 1) u ++ ")"
showType i (PairType _ t u) =
  "(" ++ showType (i - 1) t ++ ", " ++ showType (i - 1) u ++ ")"
showType i (Datatype _ m) = "[" ++ showDatatype i m ++ "]"
  -- Session types
showType i (Semi _ t u) =
  "(" ++ showType (i - 1) t ++ ";" ++ showType (i - 1) u ++ ")"
showType i (Choice _ v m) = showChoiceView v ++ "{" ++ showChoice i m ++ "}"
showType i (Forall _ b t) = "∀" ++ show b ++ "=>" ++ showType (i - 1) t
-- showType i t@(Rec _ _ _)     = showType (i-1) (Subs.unfold t)
showType i (Rec _ xk t) =
  "(rec " ++ show xk ++ "." ++ showType (i - 1) t ++ ")" -- for testing purposes
  -- Type operators
showType i (Dualof _ t) = "(dualof " ++ showType (i - 1) t ++ ")"

showNTupleType :: Int -> Type -> String
showNTupleType i (PairType _ t u) = showType (i - 1) t ++ showType (i - 1) u
showNTupleType i t                = showType i t

showDatatype :: Int -> TypeMap -> String
showDatatype i m = intercalate " | " $ Map.foldrWithKey
  (\c t acc -> (show c ++ showAsSequence t) : acc)
  []
  m
 where
  showAsSequence :: Type -> String
  showAsSequence (Fun _ _ t u) = " " ++ showType (i - 1) t ++ showAsSequence u
  showAsSequence _             = ""

showChoice :: Int -> TypeMap -> String
showChoice i m = intercalate ", " $ Map.foldrWithKey
  (\c t acc -> (show c ++ ": " ++ showType (i - 1) t) : acc)
  []
  m

instance Show TypeBind where
  show (TypeBind _ x t) = show x ++ ":" ++ show t

-- Type Schemes

-- instance Show TypeScheme where
--   show (TypeScheme _ [] t) = show t
--   show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
--     where bindings = intercalate ", " (map show bs)

-- Expressions

instance Show Expression where
--  show = showExp 4
  show = showExp 44

showExp :: Int -> Expression -> String
  -- Basic values
showExp _ (Unit _       ) = "()"
showExp _ (Integer   _ i) = show i
showExp _ (Character _ c) = show c
showExp _ (Boolean   _ b) = show b
  -- Variable
showExp _ (ProgVar   _ x) = show x
  -- Depth reached
showExp 0 _               = ".."
  -- Abstraction intro and elim
showExp i (Abs _ m b e) =
  "(λ"
    ++ show b
    ++ showArrow m
    ++ showExp (i - 1) e
    ++ ")"
showExp i (App _ e1 e2) =
  "(" ++ showExp (i - 1) e1 ++ " " ++ showExp (i - 1) e2 ++ ")"
  -- Pair intro and elim
showExp i (Pair _ e1 e2) =
  "(" ++ showExp (i - 1) e1 ++ ", " ++ showExp (i - 1) e2 ++ ")"
showExp i (BinLet _ x y e1 e2) =
  "(let "
    ++ show x
    ++ ", "
    ++ show y
    ++ " = "
    ++ showExp (i - 1) e1
    ++ " in "
    ++ showExp (i - 1) e2
    ++ ")"
  -- Datatype elim
showExp i (Case _ e m) =
  "case " ++ showExp (i - 1) e ++ " of {" ++ showFieldMap (i - 1) m ++ "}"
  -- Type Abstraction intro and elim
showExp _ (TypeApp _ x t) =
  "(" ++ show x ++ " [" ++ show t ++ "]" ++ ")"-- TODO: square brackets
showExp i (TypeAbs _ b e) =
  show "Λ" ++ show b ++ "->" ++ showExp (i - 1) e
  -- Boolean elim
showExp i (Conditional _ e e1 e2) =
  "if "
    ++ show e
    ++ " then "
    ++ showExp (i - 1) e1
    ++ " else "
    ++ showExp (i - 1) e2
  -- Let
showExp i (UnLet _ x e1 e2) =
  "(let "
    ++ show x
    ++ " = "
    ++ showExp (i - 1) e1
    ++ " in "
    ++ showExp (i - 1) e2
    ++ ")"
  -- Session types
showExp _ (New _ t _) = "new " ++ show t
showExp i (Select _ l) = "(select " ++ show l ++ ")"
showExp i (Match _ e m) =
  "match " ++ showExp (i - 1) e ++ " with {" ++ showFieldMap (i - 1) m ++ "}"

showFieldMap :: Int -> FieldMap -> String
showFieldMap i m = intercalate "; " (map showAssoc (Map.toList m))
 where
  showAssoc (b, (a, v)) =
    show b
      ++ " "
      ++ unwords (map show a)
      ++ " -> "
      ++ showExp (i - 1) v
