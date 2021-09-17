{-# LANGUAGE TupleSections, FlexibleInstances #-}
module Elaboration.Duality
  ( Duality(..)
  , dualof
  )
where

import           Data.Functor
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Syntax.Base
import qualified Syntax.Kind                   as K
import           Syntax.Expression             as E
import           Syntax.Program
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.FreestState
import           Util.Error


type Visited = Set.Set TypeVar

-- | Resolving the dualof operator

class Duality t where
  resolve :: t -> FreestState t

instance Duality TypeEnv where
  resolve = tMapM (\(k, t) -> (k, ) <$> solveType Set.empty t)

instance Duality VarEnv where
  resolve = tMapM (solveType Set.empty)

instance Duality ParseEnv where
  resolve = tMapM (\(args, e) -> (args, ) <$> resolve e)


instance Duality E.Exp where
  resolve (E.Abs p b           ) = E.Abs p <$> resolve b
  resolve (E.App  p e1 e2      ) = E.App p <$> resolve e1 <*> resolve e2
  resolve (E.Pair p e1 e2      ) = E.Pair p <$> resolve e1 <*> resolve e2
  resolve (E.BinLet p x y e1 e2) = E.BinLet p x y <$> resolve e1 <*> resolve e2
  resolve (E.Case p e m        ) = E.Case p <$> resolve e <*> resolveFieldMap m
  resolve (E.Cond p e1 e2 e3) =
    E.Cond p <$> resolve e1 <*> resolve e2 <*> resolve e3
  resolve (E.TypeApp p e t  ) = E.TypeApp p <$> resolve e <*> resolve t
  resolve (E.TypeAbs p b    ) = E.TypeAbs p <$> resolve b
  resolve (E.UnLet p x e1 e2) = E.UnLet p x <$> resolve e1 <*> resolve e2
  resolve (E.New p t u      ) = E.New p <$> resolve t <*> resolve u
  resolve e                   = return e

-- This should be an instance but it overlaps with that one of ParseEnv
resolveFieldMap :: FieldMap -> FreestState FieldMap
resolveFieldMap = mapM (\(xs, e) -> (xs, ) <$> resolve e)

instance Duality (K.Bind Exp) where
  resolve (K.Bind p a k e) = K.Bind p a k <$> resolve e

instance Duality E.Bind where
  resolve (E.Bind p m a t e) = E.Bind p m a <$> resolve t <*> resolve e

instance Duality T.Type where
  resolve = solveType Set.empty

solveType :: Visited -> T.Type -> FreestState T.Type
-- Functional Types
solveType v (T.Arrow p pol t u) =
  T.Arrow p pol <$> solveType v t <*> solveType v u
solveType v (T.Pair p t u     ) = T.Pair p <$> solveType v t <*> solveType v u
solveType v (T.Variant p m   ) = T.Variant p <$> tMapM (solveType v) m
-- Session Types
solveType v (T.Semi    p t   u) = T.Semi p <$> solveType v t <*> solveType v u
solveType v (T.Message p pol t) = T.Message p pol <$> solveType v t
solveType v (T.Choice  p pol m) = T.Choice p pol <$> tMapM (solveType v) m
-- Polymorphism and recursive types
solveType v (T.Forall p (K.Bind p' a k t)) =
  T.Forall p . K.Bind p' a k <$> solveType v t
solveType v (  T.Rec    p b) = T.Rec p <$> solveBind solveType v b
-- Dualof
solveType v d@(T.Dualof p t) = addDualof d >> solveDual v (changePos p t)
-- Var, Int, Char, Bool, Unit, Skip
solveType _ t                = pure t


solveDual :: Visited -> T.Type -> FreestState T.Type
-- Session Types
solveDual _ t@T.Skip{}          = pure t
solveDual v (T.Semi    p t   u) = T.Semi p <$> solveDual v t <*> solveDual v u
solveDual v (T.Message p pol t) = T.Message p (dual pol) <$> solveType v t
solveDual v (T.Choice p pol m) =
  T.Choice p (dual pol) <$> tMapM (solveDual v) m
-- Recursive types
solveDual v (T.Rec p b) = T.Rec p <$> solveBind solveDual v b
solveDual v t@(T.Var p a)
  -- A recursion variable
  | a `Set.member` v = pure t
  | otherwise        = pure $ T.CoVar p a
-- Dualof
solveDual v d@(T.Dualof p t) = addDualof d >> solveType v (changePos p t)
-- Non session-types
solveDual _ t = let p = pos t in addError (DualOfNonSession p t) $> t

solveBind
  :: (Visited -> T.Type -> FreestState T.Type)
  -> Visited
  -> K.Bind T.Type
  -> FreestState (K.Bind T.Type)
solveBind solve v (K.Bind p a k t) = K.Bind p a k <$> solve (Set.insert a v) t

dual :: T.Polarity -> T.Polarity
dual T.In  = T.Out
dual T.Out = T.In


-- | Changing positions

-- Change position of a given type with a given position
changePos :: Pos -> T.Type -> T.Type
changePos p (T.Int  _         ) = T.Int p
changePos p (T.Char _         ) = T.Char p
changePos p (T.Bool _         ) = T.Bool p
changePos p (T.Unit _         ) = T.Unit p
changePos p (T.String _         ) = T.String p
changePos p (T.Arrow _ pol t u) = T.Arrow p pol t u
changePos p (T.Pair _ t u     ) = T.Pair p t u
changePos p (T.Variant _ m    ) = T.Variant p m
changePos p (T.Skip _         ) = T.Skip p
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Choice  _ pol m) = T.Choice p pol m
changePos p (T.Rec    _ xs    ) = T.Rec p xs
changePos p (T.Forall _ xs    ) = T.Forall p xs
changePos p (T.Var    _ x     ) = T.Var p x
changePos p (T.Dualof _ t     ) = T.Dualof p t
changePos p (T.CoVar _ t     ) = T.CoVar p t



dualof :: T.Type -> T.Type
-- Session Types
dualof (T.Semi    p t   u) = T.Semi p (dualof t) (dualof u)
dualof (T.Message p pol t) = T.Message p (dual pol) (dualof t)
dualof (T.Choice p pol m) = T.Choice p (dual pol) (Map.map dualof m)
dualof (T.Rec p b) = T.Rec p (dualBind  b)
  where dualBind (K.Bind p a k t) = K.Bind p a k (dualof t) 
dualof (T.Dualof _ t) = dualof t
-- Non session-types & Skip
dualof t = t



