{- |
Module      :  Syntax.Expressions
Description :  The expressions in the language
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
  , Bind(..)
  )
where

import           Syntax.Base
import           Syntax.ProgramVariable
import qualified Syntax.Kind                   as K ( Bind )
import qualified Syntax.Type                   as T
import qualified Data.Map.Strict               as Map

data Exp =
  -- Basic values
    Unit Pos
  | Int Pos Int
  | Char Pos Char
  | Bool Pos Bool
  | String Pos String
  -- List                           -- native_lists
  | Nil Pos
  | List Pos Exp Exp
  -- Variable
  | Var Pos ProgVar
  -- Abstraction intro and elim
  | Abs Pos Bind        -- λ x:T -> e, λ x:T -o e
  | App Pos Exp Exp     -- e1 e2
  -- Pair intro and elim
  | Pair Pos Exp Exp
  | BinLet Pos ProgVar ProgVar Exp Exp
  -- Datatype elim
  | Case Pos Exp FieldMap
  -- Type Abstraction intro and elim
  | TypeAbs Pos (K.Bind Exp)   -- Λ a:k => e
  | TypeApp Pos Exp T.Type     -- e[T]
  -- Boolean elim
  | Cond Pos Exp Exp Exp
  -- Let
  | UnLet Pos ProgVar Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)
  -- Session types
  | New Pos T.Type T.Type

type FieldMap = Map.Map ProgVar ([ProgVar], Exp)

instance Position Exp where
  pos (Unit p             ) = p
  pos (Int p _            ) = p
  pos (Char p _           ) = p
  pos (Bool p _           ) = p
  pos (String p _         ) = p
  pos (Nil p _            ) = p -- native_lists
  pos (List p _ _ _       ) = p -- native_lists
  pos (Var p _            ) = p
  pos (Abs p _            ) = p
  pos (UnLet p _ _ _      ) = p
  pos (App p _ _          ) = p
  pos (TypeApp p _ _      ) = p
  pos (TypeAbs p _        ) = p
  pos (Cond p _ _ _       ) = p
  pos (Pair p _ _         ) = p
  pos (BinLet p _ _ _ _   ) = p
  pos (New p _ _          ) = p
  pos (Case  p _ _        ) = p

-- Bind

data Bind = Bind Pos Multiplicity ProgVar T.Type Exp

instance Position Bind where
  pos (Bind p _ _ _ _) = p

instance Default Bind where
  omission p = Bind p Un (omission p) (T.Unit p) (Unit p)
