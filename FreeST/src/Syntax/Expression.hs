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
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Expression
  ( ExpOf(..)
  , Exp
  , FieldMap_
  , FieldMap
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K ( Kind )
import qualified Syntax.Type                   as T
import qualified Data.Map.Strict               as Map

-- Expressions
-- abstracted over the type that represents variables 
-- (usually Variable, but can be Index for NamelessExp)
data ExpOf a =
  -- Basic values
    Unit Pos
  | Int Pos Int
  | Char Pos Char
  | Bool Pos Bool
  | String Pos String
  -- Variable
  | Var Pos a
  -- Abstraction intro and elim
  | Abs Pos Multiplicity (Bind (T.TypeOf a) (ExpOf a))        -- λ x:T -> e, λ x:T -o e
  | App Pos (ExpOf a) (ExpOf a)     -- e1 e2
  -- Pair intro and elim
  | Pair Pos (ExpOf a) (ExpOf a)
  | BinLet Pos Variable Variable (ExpOf a) (ExpOf a)
  -- Datatype elim
  | Case Pos (ExpOf a) (FieldMap_ a)
  -- Type Abstraction intro and elim
  | TypeAbs Pos (Bind K.Kind (ExpOf a))   -- Λ a:k => e
  | TypeApp Pos (ExpOf a) (T.TypeOf a)     -- e[T]
  -- Boolean elim
  | Cond Pos (ExpOf a) (ExpOf a) (ExpOf a)
  -- Let
  | UnLet Pos Variable (ExpOf a) (ExpOf a) -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)
  -- Session types
  | New Pos (T.TypeOf a) (T.TypeOf a)

type FieldMap_ a = Map.Map Variable ([Variable], ExpOf a)

instance Default (Bind (T.TypeOf a) (ExpOf a)) where
  omission p = Bind p (omission p) (T.Unit p) (Unit p)

-- Named expressions
type FieldMap = FieldMap_ Variable
type Exp = ExpOf Variable

instance Position (ExpOf a) where
  pos (Unit    p        ) = p
  pos (Int     p _      ) = p
  pos (Char    p _      ) = p
  pos (Bool    p _      ) = p
  pos (String  p _      ) = p
  pos (Var     p _      ) = p
  pos (Abs     p _ _    ) = p
  pos (UnLet   p _ _ _  ) = p
  pos (App     p _ _    ) = p
  pos (TypeApp p _ _    ) = p
  pos (TypeAbs p _      ) = p
  pos (Cond    p _ _ _  ) = p
  pos (Pair    p _ _    ) = p
  pos (BinLet  p _ _ _ _) = p
  pos (New     p _ _    ) = p
  pos (Case    p _ _    ) = p
