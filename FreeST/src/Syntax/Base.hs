{- |
Module      :  Syntax.Base
Description :  The Base module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the basic structures (classes and datatypes) such as Positions and
Multiplicity, that will be used the remaining Compiler.
-}
{-# LANGUAGE InstanceSigs #-}

module Syntax.Base
  ( Pos
  , Multiplicity(..)
  , defaultPos
  , Bind(..)
  , Variable(..)
  , intern
  , mkVar
  , mkNewVar
  , Span(..)
  , defaultSpan
  , showSource
  , Located(..)
  , isWild
  , clearSource
) where

-- Position

type Pos = (Int, Int)

-- class Position t where
--   pos :: t -> Pos  

defaultPos :: Pos
defaultPos = (0, 0)

-- Span

class Located t where
  getSpan :: t -> Span t
  setSpan :: Span t -> t -> t

data Span a = Span
  { startPos     :: Pos
  , endPos       :: Pos
  , source       :: Maybe a -- Maybe (Either Exp Type)
  , defModule    :: FilePath
  }

instance Eq (Span a) where
  (Span startPos endPos _ defModule) == (Span startPos' endPos' _ defModule') =
    startPos == startPos' && endPos == endPos' && defModule == defModule'

instance Ord (Span a) where
 compare (Span startPos endPos _ defModule) (Span startPos' endPos' _ defModule') = 
  compare (startPos, endPos, defModule) (startPos', endPos', defModule')

defaultSpan :: Span a
defaultSpan = Span defaultPos defaultPos Nothing ""

showSource :: (Located a, Show a) => a -> String
showSource x = f $ getSpan x
  where
    f (Span _ _ (Just x) _) = show x
    f (Span _ _ Nothing  _) = "<no-source>" ++ show x -- DEBUG ONLY; TODO: REMOVE STRING LITERAL

-- Multiplicity for types and expressions

data Multiplicity = Un | Lin deriving Eq

-- Type and program variable
data Variable = Variable (Span Variable) String

instance Eq Variable where
  (Variable _ x) == (Variable _ y) = x == y
  
instance Ord Variable where
  (Variable _ x) <= (Variable _ y) = x <= y

-- instance Position Variable where
--   pos (Variable p _) = startPos p
  
instance Located Variable where
  getSpan (Variable p _) = p
  setSpan s (Variable _ v) = Variable s v

-- The string, internal representation of a variable
intern :: Variable -> String
intern (Variable _ x) = x

-- Making a variable from a string, type or program
mkVar :: Span a -> String -> Variable
mkVar s = Variable (clearSource s)

isWild :: Variable -> Bool
isWild (Variable _ x) = x == "_"

-- Making a new variable from a given variable. The variable is
-- unique up to the point where the integer is
mkNewVar :: Int -> Variable -> Variable
mkNewVar next (Variable p str) = Variable p (show next ++ '#' : str)

-- Bind: (λ x:t -> e), (∀ a:k . t) or (Λ a:k => e) 
data Bind a b = Bind {bSpan :: Span (Bind a b), var :: Variable, binder :: a, body :: b}



-- TEMPORARY ONLY ; REMOVE AFTER MERGE
-- used as a hack when the span does not match
--   this should be handled locally with care but to
--   streamline development this function is used
-- after everything is compiling, all references to this 
--   function should be removed (so as this function)
clearSource :: Span a -> Span b
clearSource s = s{source=Nothing}