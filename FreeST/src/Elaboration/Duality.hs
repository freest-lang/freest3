{-# LANGUAGE TupleSections, FlexibleInstances #-}
module Elaboration.Duality
  (Duality(..)) where

import           Data.Functor
import qualified Data.Set                      as Set
import           Syntax.Base
import qualified Syntax.Kind                   as K
import           Syntax.Expression
import           Syntax.Program
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.FreestState


type Visited = Set.Set TypeVar

-- | Resolving the dualof operator

class Duality t where
  resolveDualof :: t -> FreestState t

instance Duality TypeEnv where
  resolveDualof = tMapM (\(k, t) -> (k,) <$> solveType Set.empty t)

instance Duality VarEnv where
  resolveDualof = tMapM (solveType Set.empty)

instance Duality ParseEnv where
  resolveDualof = tMapM (\(args, e) -> (args,) <$> resolveDualof e)


instance Duality Exp where
  resolveDualof (Abs p b     ) = Abs p <$> resolveDualof b
  resolveDualof (App  p e1 e2) = App p <$> resolveDualof e1 <*> resolveDualof e2
  resolveDualof (Pair p e1 e2) = Pair p <$> resolveDualof e1 <*> resolveDualof e2
  resolveDualof (BinLet p x y e1 e2) =
    BinLet p x y <$> resolveDualof e1 <*> resolveDualof e2
  resolveDualof (Case p e m) = Case p <$> resolveDualof e <*> resolveFieldMap m
  resolveDualof (Cond p e1 e2 e3) =
    Cond p <$> resolveDualof e1 <*> resolveDualof e2 <*> resolveDualof e3
  resolveDualof (TypeApp p e t  ) = TypeApp p <$> resolveDualof e <*> resolveDualof t
  resolveDualof (TypeAbs p b    ) = TypeAbs p <$> resolveDualof b
  resolveDualof (UnLet p x e1 e2) = UnLet p x <$> resolveDualof e1 <*> resolveDualof e2
  resolveDualof (New p t u      ) = New p <$> resolveDualof t <*> resolveDualof u
  resolveDualof e@Select{}        = pure e
  resolveDualof (Match p e m)     = Match p <$> resolveDualof e <*> resolveFieldMap m
  resolveDualof e                 = return e

-- This should be an instance but it overlaps with that one of ParseEnv
resolveFieldMap :: FieldMap -> FreestState FieldMap
resolveFieldMap = mapM (\(xs, e) -> (xs,) <$> resolveDualof e)


-- instance Duality (K.Bind T.Type) where
--   resolveDualof (K.Bind p a k t) = K.Bind p a k <$> solveType (Set.singleton a) t

instance Duality (K.Bind Exp) where
  resolveDualof (K.Bind p a k e) = K.Bind p a k <$> resolveDualof e

instance Duality Bind where
  resolveDualof (Bind p m a k e) = Bind p m a k <$> resolveDualof e

instance Duality T.Type where
  resolveDualof = solveType Set.empty 

solveType :: Visited -> T.Type -> FreestState T.Type
-- Functional Types
solveType v (T.Fun p pol t u) = T.Fun p pol <$> solveType v t <*> solveType v u
solveType v (T.Pair p t u     ) = T.Pair p <$> solveType v t <*> solveType v u
solveType v (T.Datatype p m   ) = T.Datatype p <$> tMapM (solveType v) m
-- Session Types
solveType v (T.Semi    p t   u) = T.Semi p <$> solveType v t <*> solveType v u
solveType v (T.Message p pol t) = T.Message p pol <$> solveType v t
solveType v (T.Choice  p pol m) = T.Choice p pol <$> tMapM (solveType v) m
-- Polymorphism and recursive types
solveType v (T.Forall p (K.Bind p' a k t)) =
  T.Forall p . K.Bind p' a k <$> solveType v t
solveType v (T.Rec p b)      = T.Rec p <$> solveBindT v b
solveType _ t@T.Var{}        = pure t
-- Dualof
solveType v d@(T.Dualof p t) = addTypeName p d >> solveDual v t
-- Int, Char, Bool, Unit, Skip
solveType _ p                = pure p
  

solveDual :: Visited -> T.Type -> FreestState T.Type
-- Session Types
solveDual _ t@T.Skip{}          = pure t
solveDual v (T.Semi    p t   u) = T.Semi p <$> solveDual v t <*> solveDual v u
solveDual v (T.Message p pol t) = T.Message p (dualPol pol) <$> solveType v t
solveDual v (T.Choice p pol m) =
  T.Choice p (dualPol pol) <$> tMapM (solveDual v) m
-- Recursive types
solveDual v (T.Rec p b) = T.Rec p <$> solveBindD v b
solveDual v t@(T.Var p a)
  -- A recursion variable
  | a `Set.member` v = pure t
  | otherwise = addError
      p [Error "Cannot compute the dual of a non-recursion variable:", Error t]
    $> t
-- Dualof
solveDual v d@(T.Dualof p t) = addTypeName p d >> solveType v t
-- Non session-types
solveDual _ t =
  addError (pos t) [Error "Dualof applied to a non session type: ", Error t]
    $> t

solveBindT :: Visited -> K.Bind T.Type -> FreestState (K.Bind T.Type)
solveBindT v (K.Bind p a k t) = K.Bind p a k <$> solveType (Set.insert a v) t

solveBindD :: Visited -> K.Bind T.Type -> FreestState (K.Bind T.Type)
solveBindD v (K.Bind p a k t) = K.Bind p a k <$> solveDual (Set.insert a v) t

dualPol :: T.Polarity -> T.Polarity
dualPol T.In  = T.Out
dualPol T.Out = T.In
