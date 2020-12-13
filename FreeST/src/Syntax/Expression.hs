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
  ( Exp(..)
  , FieldMap
  , ExpEnv
  )
where

import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Kind                   as K  (Bind)
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T -- (Type, TypeBind)

data Exp =
  -- Basic values
    Unit Pos
  | Int Pos Int
  | Char Pos Char
  | Bool Pos Bool
  -- Variable
  | Var Pos ProgVar
  -- Abstraction intro and elim
  | Abs Pos Multiplicity T.Bind Exp -- λ x:T -> e
  | App Pos Exp Exp            -- e1 e2
  -- Pair intro and elim
  | Pair Pos Exp Exp
  | BinLet Pos ProgVar ProgVar Exp Exp
  -- Datatype elim
  | Case Pos Exp FieldMap
  -- Type Abstraction intro and elim
  | TypeAbs Pos K.Bind Exp     -- λ a:k => e -- Higher-order polymorphism
  | TypeApp Pos Exp T.Type         -- e[T]
  -- Boolean elim
  | Conditional Pos Exp Exp Exp
  -- Let
  | UnLet Pos ProgVar Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)
  -- Session types
  | New Pos T.Type T.Type
  | Select Pos ProgVar
  | Match Pos Exp FieldMap

type FieldMap = Map.Map ProgVar ([ProgVar], Exp)

-- The definitions of the named functions in a program
type ExpEnv = Map.Map ProgVar Exp

instance Position Exp where
  pos (Unit p             ) = p
  pos (Int p _            ) = p
  pos (Char p _           ) = p
  pos (Bool p _           ) = p
  pos (Var p _            ) = p
  pos (Abs p _ _ _        ) = p
  pos (UnLet p _ _ _      ) = p
  pos (App p _ _          ) = p
  pos (TypeApp p _ _      ) = p
  pos (TypeAbs p _ _      ) = p
  pos (Conditional p _ _ _) = p
  pos (Pair p _ _         ) = p
  pos (BinLet p _ _ _ _   ) = p
  pos (New p _ _          ) = p
  pos (Select p _         ) = p
  pos (Match p _ _        ) = p
  pos (Case  p _ _        ) = p
