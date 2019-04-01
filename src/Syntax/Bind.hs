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
( PVar
, TVar
, PBind(..)
, TBind(..)
) where 

import Parse.Lexer (Position, Pos, position)

-- The base syntactic categories of FreeST

type PVar = String  -- Program Variables: Function names and function
                    -- parameters (lower case), but also datatype
                    -- constructors and labels in session types
                    -- choices (uppercase)
type TVar = String  -- Type Variables: Recursion variables (in
                    -- rec-types) and polymorphic variables
                    -- (lowercase) and The names of types introduced
                    -- with data and type declarations (uppercase)

-- Bindings: A pair of a position and a base syntactic category. These
-- are often used as keys in maps (for kinds, for type names, for
-- program variables, for function definitions)

data PBind = PBind Pos PVar

instance Position PBind where
  position (PBind p _) = p

instance Show PBind where
  show (PBind _ x) = x

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
