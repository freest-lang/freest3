{-# LANGUAGE TupleSections, LambdaCase #-}
module Elaboration.ResolveEquations(solveEquations) where

import           Elaboration.Phase
import           Syntax.Base
import qualified Syntax.Type as T
import           Util.Error
import           Util.State
import           Validation.Rename ( isFreeIn )

import           Data.Functor
import           Data.Map.Strict as Map
import qualified Data.Set as Set



type Visited = Set.Set Variable

-- | Solve equations (Types)

solveEquations :: ElabState ()
solveEquations = buildRecursiveTypes >> solveAll >> cleanUnusedRecs

solveAll :: ElabState ()
solveAll =
  getTypes
    >>= tMapWithKeyM (\x (k, v) -> (k, ) <$> solveEq Set.empty x v)
    >>= setTypes

solveEq :: Visited -> Variable -> T.Type -> ElabState T.Type
solveEq v f (T.Labelled p s tm) = T.Labelled p s <$> mapM (solveEq v f) tm
solveEq v f (T.Arrow p m t1 t2) =
  T.Arrow p m <$> solveEq v f t1 <*> solveEq v f t2
solveEq v f (T.Semi p t1 t2) = T.Semi p <$> solveEq v f t1 <*> solveEq v f t2
solveEq v f (T.Message p pol t) = T.Message p pol <$> solveEq v f t
solveEq v f t@(T.Var p x)
  | x `Set.member` v = pure t
  | f == x = pure t
  | otherwise = getFromTypes x >>= \case
    Just tx -> solveEq (f `Set.insert` v) x (snd tx)
    Nothing -> addError (TypeVarOutOfScope p x) $> omission p
solveEq v f (T.Forall p (Bind p1 x k t)) =
  T.Forall p . Bind p1 x k <$> solveEq (x `Set.insert` v) f t
solveEq v f (T.Rec p (Bind p1 x k t)) =
  T.Rec p . Bind p1 x k <$> solveEq (x `Set.insert` v) f t
solveEq v f (T.Dualof p t) = T.Dualof p <$> solveEq v f t
solveEq _ _ p              = pure p


-- | Build recursive types

buildRecursiveTypes :: ElabState ()
buildRecursiveTypes = getTypes >>= setTypes . Map.mapWithKey buildRec
  where buildRec x (k, t) = (k, setSrc (source $ getSpan t) $ T.Rec (getSpan x) (Bind (getSpan x) x k t))

-- | Clean rec types where the variable does not occur free

cleanUnusedRecs :: ElabState ()
cleanUnusedRecs = getTypes >>= setTypes . Map.map (\(k, t) -> (k, ) $ clean t)

clean :: T.Type -> T.Type
clean (T.Rec p (Bind p' y k t))
  | y `isFreeIn` t = T.Rec p $ Bind p' y k (clean t)
  | otherwise      = clean t
clean (T.Labelled p s tm) = T.Labelled p s $ Map.map clean tm
clean (T.Arrow p m t1 t2) = T.Arrow p m (clean t1) (clean t2)
clean (T.Semi p t1 t2) = T.Semi p (clean t1) (clean t2) 
clean (T.Message p pol t) = T.Message p pol (clean t)
clean (T.Forall p (Bind p1 y k t)) = T.Forall p $ Bind p1 y k (clean t)
clean (T.Dualof p t) = T.Dualof p (clean t)
clean kt = kt
