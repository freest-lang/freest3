{-# LANGUAGE FlexibleInstances #-}
{- |
Module      :  Syntax.Show
Description :  The show module
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

Converting AST terms to strings.

Norman Ramsey, Unparsing Expressions With Prefix and Postfix Operators,
Software—Practice and Experience, 1998.
https://www.cs.tufts.edu/~nr/pubs/unparse.ps

-}

module Parse.Unparser
  ( showFieldMap
  , showBindType
  , showBindExp
  , showBindTerm
  , showModuleName
  , showModuleWithDots
  ) where

import           Syntax.Base
import           Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T

import           Data.Char ( isDigit )
import           Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import           Prelude                 hiding ( Left
                                                , Right
                                                ) -- needed for Associativity

instance Show Span where
  show (Span sp fp _)
    | sp == fp  = showPos sp
    | otherwise = showPos sp ++ "-" ++ showPos fp ++ ""
    where
      showPos (l,c) = show l ++ ":" ++ show c

showModuleName :: Span -> String
showModuleName s = showModuleWithDots (defModule s)

showModuleWithDots :: String -> String
showModuleWithDots = map (\x -> if x == '/' then '.' else x )

-- Multiplicities

-- Kind
instance Show K.Multiplicity where
  show K.Un  = "*"
  show K.Lin = "1"

-- Type & Expression (Syntax.Base)
instance Show Multiplicity where
  show Un  = "->"
  show Lin = "1->"

-- Choice view

instance Show T.View where
  show T.External = "&"
  show T.Internal = "+"

-- Message Polarity
instance Show T.Polarity where
  show T.In  = "?"
  show T.Out = "!"

-- Program and Type Variables

-- Note: show should be aligned with the creation of new variables;
-- see Syntax.Variables

instance Show Variable where
  show = showVar

showVar :: Variable -> String
showVar = dropWhile (\c -> isDigit c || c == '#') . intern
-- showVar = intern -- for testing purposes

-- Sorted variable. Either a:k or x:t (just to get the spacing right)

showSortedVar :: (Show a, Show b) => a -> b -> String
showSortedVar x t = show x ++ ":" ++ show t

-- Kind

instance Show K.Basic where
  show K.Session = "S"
  show K.Message = "M"
  show K.Top     = "T"

instance Show K.Kind where
  show (K.Kind _ p m) = show p ++ show m

-- Binds

showKind :: (Show a, Show b, Show c) => a -> b -> String -> c -> String
showKind var sort arrow term =
  showSortedVar var sort ++ spaced arrow ++ show term

-- instance Show t => Show (K.Bind t) where
--   show (K.Bind _ a k t) = showKind a k "=>" t

showBindType :: Bind K.Kind T.Type -> String
showBindType (Bind _ a k t) = showKind a k "." t -- ∀ a:k . t

showBindExp :: Bind K.Kind E.Exp -> String
showBindExp (Bind _ a k e) = showKind a k "=>" e -- Λ a:k => e

-- Type bind
showBindTerm :: Bind T.Type E.Exp -> Multiplicity -> String
showBindTerm (Bind _ x t e) m = showKind x t (show m) e -- λ x:t -> e

-- Unparsing types and expressions

data Precedence =
    PMin
  | PIn      -- in, else, match, case (expressions)
  | PNew     -- new T
  | PDisj    -- ||
  | PConj    -- &&
  | PCmp     -- comparison (relational and equality)
  | PAdd     -- +, -
  | PMult    -- *, /
  | PDot     -- μ a:k . T
  | PArrow   -- λλ a:k => e,  x:T -> e, λ x:T 1-> e, T -> T and T 1-> T and ∀ a:k . T
  | PSemi    -- T ; U
  | PMsg     -- !T and ?T
  | PDualof  -- dualof T
  | PApp     -- e1 e2
  | PMax
  deriving (Eq, Ord, Bounded)

data Associativity = Left | Right | NonAssoc deriving Eq

type Rator = (Precedence, Associativity)

type Fragment = (Rator, String)

minRator, inRator, newRator, disjRator, conjRator, cmpRator, addRator, multRator, dotRator, arrowRator, semiRator, dualofRator, appRator, msgRator, maxRator 
  :: Rator
inRator = (PIn, Right)
newRator = (PNew, NonAssoc)
disjRator = (PDisj, Left)
conjRator = (PConj, Left)
cmpRator = (PCmp, NonAssoc)
addRator = (PAdd, Left)
multRator = (PMult, Left)
dotRator = (PDot, Right)
arrowRator = (PArrow, Right)
semiRator = (PSemi, Right)
msgRator = (PMsg, Right)
dualofRator = (PDualof, Right)
appRator = (PApp, Left)
minRator = (minBound, NonAssoc)
maxRator = (maxBound, NonAssoc)

noparens :: Rator -> Rator -> Associativity -> Bool
noparens (pi, ai) (po, ao) side = pi > po || pi == po && ai == ao && ao == side

bracket :: Fragment -> Associativity -> Rator -> String
bracket (inner, image) side outer | noparens inner outer side = image
                                  | otherwise = "(" ++ image ++ ")"

class Unparse t where
  unparse :: t -> Fragment

-- Type

instance Show T.Type where
  show = snd . unparse

instance Unparse T.Type where
  unparse (T.Int  _       ) = (maxRator, "Int")
  unparse (T.Char _       ) = (maxRator, "Char")
  unparse (T.Bool _       ) = (maxRator, "Bool")
  unparse (T.String _     ) = (maxRator, "String")
  unparse (T.Unit _       ) = (maxRator, "()")
  unparse (T.Skip _       ) = (maxRator, "Skip")
  unparse (T.Var  _ a     ) = (maxRator, show a)
  unparse (T.CoVar _ a    ) = (maxRator, "dual " ++ show a)
  unparse (T.Message _ p t) = (msgRator, show p ++ m)
    where m = bracket (unparse t) Right msgRator
  unparse (T.Arrow _ m t u) = (arrowRator, l ++ spaced (show m) ++ r)
   where
    l = bracket (unparse t) Left arrowRator
    r = bracket (unparse u) Right arrowRator
  unparse (T.Pair _ t u) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
   where
    l = bracket (unparse t) Left minRator
    r = bracket (unparse u) Right minRator
  unparse (T.Almanac _ T.Variant m) = (maxRator, "[" ++ showDatatype m ++ "]")
  unparse (T.Semi _ t u  ) = (semiRator, l ++ " ; " ++ r)
   where
    l = bracket (unparse t) Left semiRator
    r = bracket (unparse u) Right semiRator
  unparse (T.Almanac _ (T.Choice v) m) =
    (maxRator, show v ++ "{" ++ showChoice m ++ "}")
  unparse (T.Forall _ b) = (arrowRator, "∀" ++ showBindType b) -- ++ "=>" ++ s)
    -- where s = bracket (unparse t) Right dotRator
  unparse (T.Rec _ b) = (dotRator, "rec " ++ showBindType b) -- xk ++ "." ++ s)
    -- where s = bracket (unparse t) Right dotRator
  unparse (T.Dualof _ t) = (dualofRator, "dualof " ++ s)
    where s = bracket (unparse t) Right dualofRator

showDatatype :: T.TypeMap -> String
showDatatype m = intercalate " | "
  $ Map.foldrWithKey (\c t acc -> (show c ++ showAsSequence t) : acc) [] m
 where
  showAsSequence :: T.Type -> String
  showAsSequence (T.Arrow _ _ t u) = " " ++ show t ++ showAsSequence u
  showAsSequence _               = ""

showChoice :: T.TypeMap -> String
showChoice m = intercalate ", "
  $ Map.foldrWithKey (\c t acc -> (show c ++ ": " ++ show t) : acc) [] m

-- Expression

instance Show Exp where
  show = snd . unparse

instance Unparse Exp where
  -- Basic values
  unparse (E.Unit _) = (maxRator, "()")
  unparse (E.Int _ i) = (maxRator, show i)
  unparse (E.Char _ c) = (maxRator, show c)
  unparse (E.Bool _ b) = (maxRator, show b)
  unparse (E.String _ s) = (maxRator, show s)
  -- Variable
  unparse (E.Var  _ x) = (maxRator, show x)
  -- Abstraction intro and elim
  unparse (E.Abs _ m b) = (arrowRator, "λ" ++ showBindTerm b m)
  unparse (E.App _ (E.App _ (E.Var p x) e1) e2) | show x == "(||)" =
   (disjRator, l ++ " || " ++ r)
   where
    l = bracket (unparse e1) Left disjRator
    r = bracket (unparse e2) Right disjRator
  unparse (E.App _ (E.App _ (E.Var p x) e1) e2) | show x == "(&&)" =
   (conjRator, l ++ " && " ++ r)
   where
    l = bracket (unparse e1) Left conjRator
    r = bracket (unparse e2) Right conjRator
  unparse (E.App _ (E.App _ (E.Var p x) e1) e2) | isCmp x =
   (cmpRator, l ++ showOp x ++ r)
   where
    l = bracket (unparse e1) Left cmpRator
    r = bracket (unparse e2) Right cmpRator
  unparse (E.App _ (E.App _ (E.Var p x) e1) e2) | isAdd x =
   (addRator, l ++ showOp x ++ r)
   where
    l = bracket (unparse e1) Left addRator
    r = bracket (unparse e2) Right addRator
  unparse (E.App _ (E.App _ (E.Var p x) e1) e2) | isMult x =
   (multRator, l ++ showOp x ++ r)
   where
    l = bracket (unparse e1) Left multRator
    r = bracket (unparse e2) Right multRator
  unparse (E.App _ e1 e2) = (appRator, l ++ " " ++ r)
   where
    l = bracket (unparse e1) Left appRator
    r = bracket (unparse e2) Right appRator
  -- Pair intro and elim
  unparse (E.Pair _ e1 e2) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
   where
    l = bracket (unparse e1) Left minRator
    r = bracket (unparse e2) Right minRator
  unparse (E.BinLet _ x y e1 e2) =
    (inRator, "let " ++ p ++ " = " ++ l ++ " in " ++ r)
   where
    p = "(" ++ show x ++ ", " ++ show y ++ ")"
    l = bracket (unparse e1) Left inRator
    r = bracket (unparse e2) Right inRator
  -- Datatype elim
  unparse (E.Case _ e m) =
    (inRator, "case " ++ s ++ " of {" ++ showFieldMap m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator
  -- TODOX REMOVE DEBUGGING
  unparse (E.CaseP _ e m) =
    (inRator, "case " ++ s ++ " of {" ++ showFieldMapP m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator
  -- Type Abstraction intro and elim
  unparse (E.TypeApp _ x t) = (appRator, show x ++ " @" ++ t')
    where t' = bracket (unparse t) Right appRator
  unparse (E.TypeAbs _ b) = (arrowRator, "Λ" ++ showBindExp b)
  -- Boolean elim
  unparse (E.Cond _ e1 e2 e3) =
    (inRator, "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3)
   where
    s1 = bracket (unparse e1) Left inRator
    s2 = bracket (unparse e2) NonAssoc inRator
    s3 = bracket (unparse e3) Right inRator
  -- Unary Let
  unparse (E.New _ t _) = (newRator, "new " ++ show t)
  -- Session expressions
  unparse (E.UnLet _ x e1 e2) =
    (inRator, "let " ++ show x ++ " = " ++ l ++ " in " ++ r)
   where
    l = bracket (unparse e1) Left inRator
    r = bracket (unparse e2) Right inRator

showFieldMap :: FieldMap -> String
showFieldMap m = intercalate "; " $ map showAssoc (Map.toList m)
 where
  showAssoc (b, (a, v)) =
    show b ++ " " ++ unwords (map show a) ++ " -> " ++ show v

-- TODOX for debugging
showFieldMapP :: FieldMapP -> String
showFieldMapP m = intercalate "; " $ map show m
 where
  showAssoc (b, (a, v)) =
    show b ++ " " ++ unwords (map show a) ++ " -> " ++ show v

isOp :: [String] -> Variable -> Bool
isOp ops x = show x `elem` ops

isCmp :: Variable -> Bool
isCmp = isOp ["(<)", "(>)", "(<=)", "(>=)", "(==)", "(/=)"]

isAdd :: Variable -> Bool
isAdd = isOp ["(+)", "(-)"]

isMult :: Variable -> Bool
isMult = isOp ["(*)", "(/)"]

showOp :: Variable -> String
showOp x = spaced $ tail (init $ show x)

spaced :: String -> String
spaced s = ' ' : s ++ " "

-- VarEnv

instance {-# OVERLAPPING #-} Show VarEnv where
  show venv = "[" ++ intercalate "\n\t\t   ," (venvToList venv) ++ "]"

venvToList :: VarEnv -> [String]
venvToList =
  Map.foldrWithKey (\k v acc -> showSortedVar k v : acc) []
