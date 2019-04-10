{- |
Module      :  Position
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Bind
(
-- Program variables
  PVar
, mkPVar
, mkNonBindablePVar
-- Type variables
, TVar
, mkTVar
, mkNonBindableTVar
-- Binds
, PBind (..)
, TBind (..)
, Default (..)
) where 

import           Parse.Lexer (Position, Pos, position)
import           Data.Char (isDigit)

-- The base syntactic categories of FreeST

-- Program Variables: Function names and function parameters (lower
-- case), but also datatype constructors and labels in session types
-- choices (uppercase)

newtype PVar = PVar { getPVar :: String } deriving (Eq, Ord)

instance Show PVar where
 show = showVar . getPVar

-- Use this for function names and function parameters (lower case)
mkPVar :: Int -> String -> PVar
mkPVar next id = PVar (mkVar next id)

-- Use this for datatype constructors and labels in session types
-- choices (uppercase)
mkNonBindablePVar :: String -> PVar
mkNonBindablePVar = PVar

instance Default PVar where
  omission _= mkPVar 9999 "#"

-- Type Variables: Recursion variables (in rec-types) and polymorphic
-- variables (lowercase) and the names of types introduced with type
-- and data declarations (uppercase)

-- type TVar = String

newtype TVar = TVar { getTVar :: String } deriving (Eq, Ord)

instance Show TVar where
 show = showVar . getTVar

-- Use this for recursion variables (in rec-types) and polymorphic
-- variables (lowercase)
mkTVar :: Int -> String -> TVar
mkTVar next id = TVar (mkVar next id)

-- Use this for the names of types introduced with type and data
-- declarations (uppercase)
mkNonBindableTVar :: String -> TVar
mkNonBindableTVar = TVar

showVar :: String -> String
showVar id
   | isDigit (head id) = tail $ dropWhile (isDigit) id
   | otherwise         = id

mkVar :: Int -> String -> String
mkVar next id = show next ++ '_' : id

-- Bindings: A pair composed of a position and a base syntactic
-- category. These are often used as keys in maps (for kinds, for type
-- names, for program variables, for function definitions, for fields
-- in types)

data PBind = PBind Pos PVar

instance Position PBind where
  position (PBind p _) = p

instance Show PBind where
  show (PBind _ x) = show x

instance Eq PBind where
  (PBind _ x) == (PBind _ y) = x == y
  
instance Ord PBind where
  (PBind _ x) <= (PBind _ y) = x <= y

data TBind = TBind Pos TVar

instance Position TBind where
  position (TBind p _) = p

instance Show TBind where
  show (TBind _ x) = show x

instance Eq TBind where
  (TBind _ x) == (TBind _ y) = x == y
  
instance Ord TBind where
  (TBind _ x) <= (TBind _ y) = x <= y


-- Default kinds, types, and type schemes. Used when the compiler
-- needs to fill an error
class Default t where
  omission :: Pos -> t
  
