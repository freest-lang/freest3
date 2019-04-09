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
( PVar(..)
, TVar
, PBind(..)
, TBind(..)
, Default (..)
) where 

import Parse.Lexer (Position, Pos, position)
import Data.Char (isDigit)

-- The base syntactic categories of FreeST

newtype PVar = PVar { getPVar :: String } deriving (Eq, Ord)

instance Show PVar where
 show v = dropWhile (isDigit) (getPVar v)

--type PVar = String  -- Program Variables: Function names and function
                    -- parameters (lower case), but also datatype
                    -- constructors and labels in session types
                    -- choices (uppercase)
type TVar = String  -- Type Variables: Recursion variables (in
                    -- rec-types) and polymorphic variables
                    -- (lowercase) and the names of types introduced
                    -- with type and data declarations (uppercase)

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
  show (TBind _ x) = x

instance Eq TBind where
  (TBind _ x) == (TBind _ y) = x == y
  
instance Ord TBind where
  (TBind _ x) <= (TBind _ y) = x <= y


-- Default kinds, types, and type schemes. Used when the compiler
-- needs to fill an error
class Default t where
  omission :: Pos -> t
  
