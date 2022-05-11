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
  resolve = tMapM (\(k, t) -> (k, ) <$> solveType Set.empty Map.empty t)

instance ResolveDuality VarEnv where
  resolve = tMapM (solveType Set.empty Map.empty)

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

instance ResolveDuality (Bind K.Kind Exp) where
  resolve (Bind p a k e) = Bind p a k <$> resolve e

instance ResolveDuality (Bind T.Type E.Exp) where
  resolve (Bind p a t e) = Bind p a <$> resolve t <*> resolve e

instance ResolveDuality T.Type where
  resolve = solveType Set.empty Map.empty

type VisitedRecVars = Set.Set Variable
type VisitedRecBody = Map.Map Variable T.Type

solveType :: VisitedRecVars -> VisitedRecBody -> T.Type -> FreestState T.Type
-- Almanac (Variant, Choice, Record)
solveType vs v (T.Almanac p s m) = 
  T.Almanac p s <$> tMapM (solveType vs v) m
-- Functional Types
solveType vs v (T.Arrow p pol t u) =
  T.Arrow p pol <$> solveType vs v t <*> solveType vs v u
solveType vs v (T.Pair p t u     ) =
  T.Pair p <$> solveType vs v t <*> solveType vs v u
-- Session Types
solveType vs v (T.Semi    p t   u) =
  T.Semi p <$> solveType vs v t <*> solveType vs v u
solveType vs v (T.Message p pol t) =
  T.Message p pol <$> solveType vs v t
-- Polymorphism and recursive types
solveType vs v (T.Forall p (Bind p' a k t)) =
  T.Forall p . Bind p' a k <$> solveType vs v t
solveType vs v (T.Rec p b) =
  T.Rec p <$> solveBind solveType vs v b
-- Dualof
solveType vs v d@(T.Dualof p var@(T.Var p' x)) = case v Map.!? x of
  Just t -> do
    addDualof d
    fv <- freshTVar "#X" p'
    let b = Bind p' fv (K.sl p') (changePos p (subs (T.Var p' fv) x t))
    T.Rec p <$> solveBind solveDual vs (x `Map.delete` v) b
  Nothing -> addDualof d >> solveDual vs v (changePos p var)
solveType vs v d@(T.Dualof p t) =
  addDualof d >> solveDual vs v (changePos p t)
-- Var, Int, Char, Bool, Unit, Skip
solveType _ _ t                = pure t


solveDual :: VisitedRecVars -> VisitedRecBody -> T.Type -> FreestState T.Type
-- Session Types
solveDual _ _ t@T.Skip{}          = pure t
solveDual vs v (T.Semi    p t   u) =
  T.Semi p <$> solveDual vs v t <*> solveDual vs v u
solveDual vs v (T.Message p pol t) =
  T.Message p (dualof pol) <$> solveType vs v t
solveDual vs v (T.Almanac p (T.Choice view) m) =
  T.Almanac p (T.Choice $ dualof view) <$> tMapM (solveDual vs v) m
-- Recursive types
solveDual vs v (T.Rec p b) =
  T.Rec p <$> solveBind solveDual vs v b
solveDual vs _ t@(T.Var p a)
  | a `Set.member` vs = pure t -- A recursion variable
  | otherwise         = pure $ T.CoVar p a -- A polymorphic variable
-- Dualof
solveDual vs v d@(T.Dualof p t@(T.Var _ a))
  | a `Map.member` v =
     addDualof d >> solveType vs (a `Map.delete` v) (changePos p (v Map.! a))
  | otherwise = addDualof d >> solveType vs v (changePos p t)
solveDual vs v d@(T.Dualof p t) =
  addDualof d >> solveType vs v (changePos p t)
-- Non session-types
solveDual _ _ t = addError (DualOfNonSession (getSpan t) t) $> t

solveBind
  :: (VisitedRecVars -> VisitedRecBody -> T.Type -> FreestState T.Type)
  -> VisitedRecVars
  -> VisitedRecBody
  -> Bind K.Kind T.Type
  -> FreestState (Bind K.Kind T.Type)
solveBind solve vs v (Bind p a k t) =
  Bind p a k <$> solve (Set.insert a vs) (Map.insert a t v) t

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
changePos p (T.Almanac _ s m  ) = T.Almanac p s m
changePos p (T.Skip _         ) = T.Skip p
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Rec    _ xs    ) = T.Rec p xs
changePos p (T.Forall _ xs    ) = T.Forall p xs
changePos p (T.Var    _ x     ) = T.Var p x
changePos p (T.Dualof _ t     ) = T.Dualof p t
changePos p (T.CoVar  _ t     ) = T.CoVar p t


