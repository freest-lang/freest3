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

module Syntax.Types
( BasicType(..)
, TypeMap(..)
, Polarity(..)
, Type(..)
, Dual(..)
, subs
, unfold
) where

import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables (ProgVar)
import           Syntax.Base
-- import           Syntax.Bind
-- import           Syntax.Kinds
-- import           Parse.Lexer (Position, Pos, position)
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- The dual function on types, etc
class Dual t where
  dual :: t -> t

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

instance Show Polarity where
  show In  = "?"
  show Out = "!"

instance Dual Polarity where
  dual In  = Out
  dual Out = In

-- BASIC TYPES

data BasicType =
    IntType
  | CharType
  | BoolType
  | UnitType
  deriving (Eq, Ord)

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- TYPES

data Type =
  -- Functional types
    Basic Pos BasicType
  | Fun Pos Multiplicity Type Type
  | PairType Pos Type Type
  | Datatype Pos TypeMap
  -- Session types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity BasicType
  | Choice Pos Polarity TypeMap
  | Rec Pos TypeVarBind Type
  -- Functional or session
  | TypeVar Pos TypeVar  -- a recursion variable if bound, polymorphic otherwise
  -- Type operators
  | TypeName Pos TypeVar -- a named type, to be looked upon in a map of type names to types
  | Dualof Pos Type      -- to be expanded into a session type
--  deriving Ord -- Why needeed?

type TypeMap = Map.Map ProgVar Type

instance Eq Type where -- Type equality, up to alpha-conversion
  t == u = equalTypes Map.empty t u 

equalTypes :: Map.Map TypeVar TypeVar -> Type -> Type -> Bool
  -- Functional types
equalTypes s (Basic _ x)      (Basic _ y)      = x == y
equalTypes s (Fun _ m t u)    (Fun _ n v w)    = m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType _ t u) (PairType _ v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype _ m1)  (Datatype _ m2)  = equalMaps s m1 m2
  -- Session types
equalTypes s (Skip _)         (Skip _)         = True
equalTypes s (Semi _ t1 t2)   (Semi _ u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Message _ p x)  (Message _ q y)  = p == q && x == y
equalTypes s (Choice _ v1 m1) (Choice _ v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes s (Rec _ (TypeVarBind _ x _) t) (Rec _ (TypeVarBind _ y _) u) = equalTypes (Map.insert x y s) t u
  -- Functional or session
equalTypes s (TypeVar _ x)    (TypeVar _ y)    = equalVars (Map.lookup x s) x y
  -- Type operators
equalTypes s (Dualof _ t)     (Dualof _ u)     = t == u
equalTypes s (TypeName _ x)       (TypeName _ y)       = x == y
  -- Otherwise
equalTypes _ _              _                  = False

equalVars :: Maybe TypeVar -> TypeVar -> TypeVar -> Bool
equalVars Nothing  y z = y == z
equalVars (Just x) _ z = x == z

equalMaps :: Map.Map TypeVar TypeVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

instance Show Type where
  -- Functional types
  show (Basic _ b)      = show b
  show (Fun _ m t u)    = "(" ++ show t ++ showFunOp m ++ show u ++ ")"
  show (PairType _ t u) = "(" ++ show t ++ ", " ++ show u ++ ")"
  show (Datatype _ m)   = "["++ showMap m ++"]"
  -- Session types
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Choice _ v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Rec _ x t)      = "(rec " ++ show x ++ " . " ++ show t ++ ")"
  -- Functional or session
  show (TypeVar _ x)    = show x
  -- Type operators
  show (Dualof _ s)     = "(dualof " ++ show s ++ ")"
  show (TypeName _ x)       = show x
  
showFunOp :: Multiplicity -> String
showFunOp Lin = " -o "
showFunOp Un  = " -> "

showChoice :: Polarity -> String
showChoice In  = "&"
showChoice Out = "+"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (b, v) = show b ++ ": " ++ show v

instance Position Type where
  -- Functional types
  position (Basic p _)     = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  -- Session types
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  position (Rec p _ _)      = p
  -- Functional or session
  position (TypeVar p _)    = p
  -- Type operators
  position (Dualof p _)     = p
  position (TypeName p _)   = p

instance Dual Type where
  -- Session types
  dual (Semi p t1 t2)  = Semi p (dual t1) (dual t2)
  dual (Message p v b) = Message p (dual v) b
--  dual (Choice p v m)  = Choice p (dual v) (Map.map dual m)
  dual (Choice p v m)  = Choice p (dual v) (Map.map (Dualof p) m) -- The lazy version, hopefully faster
--  dual (Rec p x t)     = Rec p x (dual t)
  dual (Rec p x t)     = Rec p x (Dualof p t) -- The lazy version, hopefully faster
  -- Type operators
  dual (Dualof _ t)    = t
  -- Functional types, Skip, TypeVar, TypeName
  dual t               = t

instance Default Type where
  omission p = Basic p UnitType

-- Unfolding, Substituting

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold t@(Rec p (TypeVarBind _ b _) u) = subs t b u

-- [u/x]t, substitute u for x on t
subs :: Type -> TypeVar -> Type -> Type 
  -- Functional types
subs t x (Fun p m t1 t2)    = Fun p m (subs t x t1) (subs t x t2)
subs t x (PairType p t1 t2) = PairType p (subs t x t1) (subs t x t2)
subs t x (Datatype p m)     = Datatype p (Map.map(subs t x) m)
  -- Session types
subs t x (Semi p t1 t2)     = Semi p (subs t x t1) (subs t x t2)
subs t x (Choice p v m)     = Choice p v (Map.map(subs t x) m)
subs t1 x (Rec p b t2)      = Rec p b (subs t1 x t2) -- Assume types were renamed
  -- | y == x                  = u
  -- | otherwise               = Rec p y (subs t1 x t2)
  -- Functional or session
subs t x u@(TypeVar _ y)
  | y == x                  = t
  | otherwise               = u
  -- Type operators  
subs t x (Dualof p t1)      = Dualof p (subs t x t1)
  -- Otherwise: Basic, Skip, Message, TypeName
subs _ _ t                  = t
