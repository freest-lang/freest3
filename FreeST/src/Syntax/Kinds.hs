{- |
Module      :  Syntax.Kinds
Description :  The kind of a type
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a kind and the relational order between kinds. It also defines the
subkinding relation, the least upper bound of two kinds and other functions to
manipulate prekinds and multiplicities.
-}

module Syntax.Kinds
( PreKind (..)
, KindBind(..)
, Kind (..)
, KindEnv
, kindTL
, kindTU
, kindSL
, kindSU
, kindMU
, kindML
, isSession
, (<:)
, join
, isLin
, isUn
, fromKindBinds
) where

import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

-- Prekinds

data PreKind = Msg | Session | Functional deriving Eq

instance Ord PreKind where
   Session <= Functional = True
   _       <= _          = False

-- Kinds

data Kind = Kind Pos PreKind Multiplicity 
          | KindArrow Pos Kind Kind
          deriving Ord -- TODO: I wish we do not need this

instance Eq Kind where
  (Kind _ p n) == (Kind _ q m) = (p, n) == (q, m)

-- Abbreviations for the four kinds
kindTL, kindTU, kindSL, kindSU, kindMU, kindML :: Pos -> Kind
kindTL p = Kind p Functional Lin
kindTU p = Kind p Functional Un
kindSL p = Kind p Session Lin
kindSU p = Kind p Session Un
kindMU p = Kind p Msg Un
kindML p = Kind p Msg Lin

-- The subkinding relation. Note that Kind is a partial order, hence
-- should *not* be an instance class Ord.
--    TL
--   /  \
-- TU    SL
--   \  /
--    SU
(<:) :: Kind -> Kind -> Bool
(Kind _ Msg m1)     <: (Kind _ Functional m2) = m1 <= m2
(Kind _ Session m1) <: (Kind _ Functional m2) = m1 <= m2
(KindArrow _ k1 k2) <: (KindArrow _ k1' k2')  = k1' <= k1 && k2 <: k2'
k1                  <: k2                     = preKind k1 == preKind k2 && mult k1 <= mult k2

mult :: Kind -> Multiplicity
mult (Kind _ _ m) = m
mult (KindArrow _ k1 k2) = max (mult k1) (mult k2)

preKind :: Kind -> PreKind
preKind (Kind _ k _) = k

-- The least upper bound of two kinds
join :: Kind -> Kind -> Kind
join (Kind p Functional Un) (Kind _ Session   Lin) = kindTL p
join (Kind p Session   Lin) (Kind _ Functional Un) = kindTL p
join k1                     k2                     = if k1 <: k2 then k2 else k1

-- The kind of conventional (non linear, not sessions) functional
-- programming languages (Alternative: the kind that sits at the top
-- of the hierarchy)
instance Default Kind where
  omission = kindTU

isSession :: Kind -> Bool
isSession = (<: kindSL defaultPos)

isLin :: Kind -> Bool
isLin (Kind _ _ m) = m == Lin

isUn :: Kind -> Bool
isUn = not . isLin

instance Position Kind where
  position (Kind p _ _) = p

-- Kind environments

type KindEnv = Map.Map TypeVar Kind

-- Binding type variables to kinds

data KindBind = KindBind Pos TypeVar Kind deriving (Eq, Ord)

instance Position KindBind where
  position (KindBind p _ _) = p

fromKindBinds :: [KindBind] -> KindEnv
fromKindBinds = foldr (\(KindBind _ x k) env -> Map.insert x k env) Map.empty
