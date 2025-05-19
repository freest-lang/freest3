{-# LANGUAGE LambdaCase #-}
module Inference.ConstraintKinding (constraintKinding) where

import           Inference.Constraint
import           Inference.Phase
import           Kinding.Norm
import           Kinding.Kinding
import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Functor
import           Control.Monad
import           Util.State

constraintKinding :: K.KindEnv -> T.Type -> InfState K.Kind
constraintKinding kEnv = cg (Map.keysSet kEnv) kEnv

cg :: K.PolyVars -> K.KindEnv -> T.Type -> InfState K.Kind
cg pEnv kEnv (T.Rec p (Bind _ a k@(K.Kind _ Un K.Session) t@(T.Semi _ T.Message{} (T.Var _ b))))
  | a == b  = cg pEnv (Map.insert a k kEnv) t $> K.ua p
cg _ _ (T.Rec p (Bind _ a (K.Kind _ Un K.Session) (T.Labelled _ (T.Choice _) _ m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ K.ua p
cg _ _ (T.Int s) = return $ K.ut s 
cg _ _ (T.Float s) = return $ K.ut s 
cg _ _ (T.Char s) = return $ K.ut s 
cg _ _ (T.String s) = return $ K.ut s 
cg _ _ (T.Skip s) = return $ K.us s 
cg _ _ (T.End s _ _) = return $ K.la s 
cg pEnv kEnv (T.Message s _ _ t) = cg pEnv kEnv t $> K.ls s
cg pEnv kEnv (T.Dualof s t) = cg pEnv kEnv t $> K.ls s
cg pEnv kEnv (T.Labelled s T.Choice{} _ m) = do
  kl <- foldM (\acc t -> cg pEnv kEnv t >>= \k -> return $ k : acc) [] m
  mapM_ (\k -> addConstraint (KindC k (K.ls (getSpan k)))) kl
  pk <- freshPKVar s
  addConstraint (PKJoinC pk kl) 
  return $ K.Kind s Lin (K.PKVar pk)
cg pEnv kEnv (T.Semi s t1 t2) = do
  k1 <- cg pEnv kEnv t1
  k2 <- cg pEnv kEnv t2
  addConstraint (KindC k1 (K.ls (getSpan k1)))
  addConstraint (KindC k2 (K.ls (getSpan k2)))
  mv <- freshMultVar s
  addConstraint (MultC mv [k1,k2])
  pk <- freshPKVar s
  addConstraint (PKMeetC pk [k1,k2]) 
  return $ K.Kind s (MultVar mv) (K.PKVar pk)
cg _ _ (T.Labelled s T.Record _ m) | Map.null m = return $ K.ut s -- Unit
cg pEnv kEnv (T.Arrow s m l1 l2 t1 t2) = cg pEnv kEnv t1 >>  cg pEnv kEnv t2 $> K.Kind s m K.Top
cg pEnv kEnv (T.Labelled s _ _ m) = do
  kl <- foldM (\acc t -> cg pEnv kEnv t >>= \k -> return $ k : acc) [] m
  mv <- freshMultVar s
  addConstraint (MultC mv kl)
  unless (all isVar kl) $ 
    addConstraint $ MultC mv (foldl (\acc k -> if isVar k then acc else k:acc ) [] kl)
  return $ K.Kind s (MultVar mv) K.Top
cg pEnv kEnv (T.Forall s b) = do
  k' <- cg (Set.insert (var b) pEnv) (Map.insert (var b) (binder b) kEnv) (body b)
  mv <- freshMultVar s
  addConstraint $ MultC mv [k']
  return $ K.Kind s (MultVar mv) K.Top
cg _ kEnv (T.Var p x) = 
  case kEnv Map.!? x of
    Just k -> return k
    Nothing -> addError (TypeVarNotInScope p x) $> omission p
cg pEnv kEnv mu@(T.Rec _ b) = do
  checkContractive pEnv (var b) (body b)
  k' <- cg pEnv (Map.insert (var b) (binder b) kEnv) (body b)
  addConstraint (KindC k' (binder b))
  when (isVar (binder b)) (addConstraint (KindC (binder b) k'))
  if normed pEnv mu then return k'
  else do
    mv <- freshMultVar (getSpan $ binder b)
    addConstraint (MultC mv [k'])
    return $ K.Kind (getSpan $ binder b) (MultVar mv) K.Absorb

isVar :: K.Kind -> Bool
isVar (K.Kind _ MultVar{} K.PKVar{}) = True
isVar (K.Kind _ _ K.PKVar{}) = True
isVar (K.Kind _ MultVar{} _) = True
isVar _ = False

isProperK :: K.Kind -> Bool
isProperK (K.Kind _ _ K.PKVar{}) = False 
isProperK (K.Kind _ MultVar{} _) = False
isProperK _ = True
