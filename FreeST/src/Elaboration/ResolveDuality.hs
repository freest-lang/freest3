{-# LANGUAGE TupleSections, FlexibleInstances #-}
module Elaboration.ResolveDuality
  ( ResolveDuality(..)
  )
where


import           Elaboration.Duality
import           Syntax.Base
import qualified Syntax.Kind as K
import           Syntax.Expression as E
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.FreestState
import           Util.Error
import           Validation.Substitution

import           Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Resolving the dualof operator

class ResolveDuality t where
   resolve :: t -> FreestState t

instance ResolveDuality TypeEnv where
  resolve = tMapM (\(k, t) -> (k, ) <$> solveType Set.empty t)

instance ResolveDuality VarEnv where
  resolve = tMapM (solveType Set.empty)

instance ResolveDuality ParseEnv where
  resolve = tMapM (\(args, e) -> (args, ) <$> resolve e)


instance ResolveDuality E.Exp where
  resolve (E.Abs p m b         ) = E.Abs p m <$> resolve b
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

instance (ResolveDuality a, ResolveDuality b) => ResolveDuality (Bind a b) where
  resolve (Bind p a t e) = Bind p a <$> resolve t <*> resolve e

-- We need this one, to have the previous one working. In case E.TAbs a is a Kind 
instance ResolveDuality K.Kind where
  resolve k = pure k

instance ResolveDuality T.Type where
  resolve = solveType Set.empty

type Visited = Set.Set Variable

solveType :: Visited -> T.Type -> FreestState T.Type
-- Functional Types
solveType v (T.Arrow p pol t u) =
  T.Arrow p pol <$> solveType v t <*> solveType v u
solveType v (T.Pair p t u     ) = T.Pair p <$> solveType v t <*> solveType v u
solveType v (T.Almanac p s m   ) = T.Almanac p s <$> tMapM (solveType v) m
-- Session Types
solveType v (T.Semi    p t   u) = T.Semi p <$> solveType v t <*> solveType v u
solveType v (T.Message p pol t) = T.Message p pol <$> solveType v t
-- Polymorphism and recursive types
solveType v (T.Forall p (Bind p' a k t)) =
  T.Forall p . Bind p' a k <$> solveType v t
solveType v (  T.Rec    p b) = T.Rec p <$> solveBind solveType v b
-- Dualof
-- solveType vs v d@(T.Dualof p var@(T.Var p' x)) = case v Map.!? x of
--   Just t -> do
--     addDualof d
--     fv <- freshTVar "#X" p'
--     let b = Bind p' fv (K.ls p') (changePos p (subs (T.Var p' fv) x t))
--     T.Rec p <$> solveBind solveDual vs (x `Map.delete` v) b
--   Nothing -> addDualof d >> solveDual vs v (changePos p var)
-- solveType vs v d@(T.Dualof p t) =
--   addDualof d >> solveDual vs v (changePos p t)
solveType v d@(T.Dualof p t) = addDualof d >> solveDual v (changePos p t)

-- Var, Int, Char, Bool, Unit, Skip, End
solveType _ t                = pure t


solveDual :: Visited -> T.Type -> FreestState T.Type
-- Session Types
solveDual _ t@T.Skip{}          = pure t
solveDual _ t@T.End{}           = pure t
solveDual v (T.Semi    p t   u) = T.Semi p <$> solveDual v t <*> solveDual v u
solveDual v (T.Message p pol t) = T.Message p (dual pol) <$> solveType v t
solveDual v (T.Almanac p (T.Choice pol) m) =
  T.Almanac p (T.Choice $ dualof pol) <$> tMapM (solveDual v) m
-- Recursive types
solveDual v t@(T.Rec p b@(Bind _ a _ _)) = do
  u <- solveDBind solveDual v b
  return $ cosubs t a (T.Rec p u)
solveDual v t@(T.Var p a)
  -- A recursion variable
  | a `Set.member` v = pure t
  | otherwise        = pure $ T.CoVar p a
-- Dualof
solveDual v d@(T.Dualof p t) = addDualof d >> solveType v (changePos p t)
solveDual v (T.CoVar p a) = pure $ T.Var p a
-- Non session-types
solveDual _ t = addError (DualOfNonSession (getSpan t) t) $> t

solveBind
  :: (Visited -> T.Type -> FreestState T.Type)
  -> Visited
  -> Bind a T.Type
  -> FreestState (Bind a T.Type)
solveBind solve v (Bind p a k t) = Bind p a k <$> solve (Set.insert a v) t

solveDBind
  :: (Visited -> T.Type -> FreestState T.Type)
  -> Visited
  -> Bind a T.Type
  -> FreestState (Bind a T.Type)
solveDBind solve v (Bind p a k t) =
  Bind p a k <$> solve (Set.insert a v) (subs (T.CoVar p a) a t)

dual :: T.Polarity -> T.Polarity
dual T.In  = T.Out
dual T.Out = T.In


-- | Changing positions

-- Change position of a given type with a given position
changePos :: Span -> T.Type -> T.Type
changePos p (T.Int    _       ) = T.Int p
changePos p (T.Char   _       ) = T.Char p
changePos p (T.Bool   _       ) = T.Bool p
changePos p (T.Unit   _       ) = T.Unit p
changePos p (T.String _       ) = T.String p
changePos p (T.Arrow _ pol t u) = T.Arrow p pol t u
changePos p (T.Pair _ t u     ) = T.Pair p t u
changePos p (T.Almanac _ s m    ) = T.Almanac p s m
changePos p (T.Skip _         ) = T.Skip p
changePos p (T.End  _         ) = T.End p
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Rec    _ xs    ) = T.Rec p xs
changePos p (T.Forall _ xs    ) = T.Forall p xs
changePos p (T.Var    _ x     ) = T.Var p x
changePos p (T.Dualof _ t     ) = T.Dualof p t
changePos p (T.CoVar _ t     ) = T.CoVar p t
