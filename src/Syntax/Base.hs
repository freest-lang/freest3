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

module Syntax.Base
( Default(..)
, Multiplicity(..)
, Pos           -- Relayed from Lexer
, Position(..)  -- Relayed from Lexer
, defaultPos    -- Relayed from Lexer
) where

import Parse.Lexer

-- Defaults for the various syntactic categories

class Default t where
  omission :: Pos -> t

-- Multiplicities for kinds, types, and expressions

data Multiplicity = Un | Lin deriving Eq

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

instance Ord Multiplicity where
  Un <= Lin = True
  _  <= _  = False
