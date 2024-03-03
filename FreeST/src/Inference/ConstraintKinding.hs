{-# LANGUAGE LambdaCase #-}
module Inference.ConstraintKinding where

import Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import Syntax.Constraint
import Inference.Phase
import Kinding.Kinding

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor
import Control.Monad
import Util.State

cg :: K.KindEnv -> T.Type -> InfState K.Kind
cg kEnv = cg' (Map.keysSet kEnv) kEnv

cg' :: K.PolyVars -> K.KindEnv -> T.Type -> InfState K.Kind
cg' pEnv kEnv (T.Rec p (Bind _ a k@(K.Kind _ Un K.Session) t@(T.Semi _ T.Message{} (T.Var _ b))))
  | a == b  = cg' pEnv (Map.insert a k kEnv) t $> K.ua p
cg' _ _ (T.Rec p (Bind _ a (K.Kind _ Un K.Session) (T.Labelled _ (T.Choice _) m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m = return $ K.ua p
cg' _ _ (T.Int s) = return $ K.ut s 
cg' _ _ (T.Float s) = return $ K.ut s 
cg' _ _ (T.Char s) = return $ K.ut s 
cg' _ _ (T.String s) = return $ K.ut s 
cg' _ _ (T.Skip s) = return $ K.us s 
cg' _ _ (T.End s _) = return $ K.la s 
cg' pEnv kEnv (T.Message s _ t) = cg' pEnv kEnv t $> K.ls s
cg' pEnv kEnv (T.Dualof s t) = cg' pEnv kEnv t $> K.ls s
cg' pEnv kEnv (T.Labelled s T.Choice{} m) = do
  kl <- foldM (\acc t -> cg' pEnv kEnv t >>= \k -> return $ k : acc) [] m
  mapM_ (\k -> addConstraint (KindC k (K.ls (getSpan k)))) kl
  pk <- freshPKVar s
  addConstraint (PKJoinC pk kl) 
  return $ K.Kind s Lin (K.PKVar pk)
cg' pEnv kEnv t@(T.Semi s t1 t2) = do
  k1 <- cg' pEnv kEnv t1
  k2 <- cg' pEnv kEnv t2

  -- if show t == "(rec TreeC:1S . &{LeafC: Skip, NodeC: TreeC ; ?Int ; TreeC}) ; a" then
  --   debugM $ "~~~~> " ++ show t
  --          ++ "      k1: " ++ show k1
  --          ++ "      k1: " ++ show k2
  -- else return ()
  
  addConstraint (KindC k1 (K.ls (getSpan k1)))
  addConstraint (KindC k2 (K.ls (getSpan k2)))
  mv <- freshMultVar s
  addConstraint (MultC mv [k1,k2])
  pk <- freshPKVar s
  addConstraint (PKMeetC pk [k1,k2]) 
  return $ K.Kind s (MultVar mv) (K.PKVar pk)

cg' _ _ (T.Labelled s T.Record m) | Map.null m = return $ K.ut s -- Unit
cg' pEnv kEnv (T.Arrow s m t1 t2) = cg' pEnv kEnv t1 >>  cg' pEnv kEnv t2 $> K.Kind s m K.Top

cg' pEnv kEnv (T.Labelled s _ m) = do
  kl <- foldM (\acc t -> cg' pEnv kEnv t >>= \k -> return $ k : acc) [] m
  mv <- freshMultVar s
  addConstraint (MultC mv kl)

  -- TODO: hack think better?
  unless (all isVar kl) $ 
    addConstraint $ MultC mv (foldl (\acc k -> if isVar k then acc else k:acc ) [] kl)
  
  return $ K.Kind s (MultVar mv) K.Top

cg' pEnv kEnv (T.Forall s b) = do -- (Bind K.Kind Type)
  k' <- cg' (Set.insert (var b) pEnv) (Map.insert (var b) (binder b) kEnv) (body b)
  mv <- freshMultVar s
  addConstraint $ MultC mv [k']
  return $ K.Kind s (MultVar mv) K.Top
cg' _ kEnv (T.Var _ x) = 
  case kEnv Map.!? x of
    Just k -> return k
    Nothing -> error $ "Var " ++ show x ++ " not in scope " -- TODO: Proper error  

cg' pEnv kEnv mu@(T.Rec _ b) = do
  checkContractive pEnv (var b) (body b)
  k' <- cg' pEnv (Map.insert (var b) (binder b) kEnv) (body b)
  addConstraint (KindC k' (binder b))
  addConstraint (KindC (binder b) k')

  if unr pEnv kEnv mu then do
    mv <- freshMultVar (getSpan $ binder b)
    addConstraint (MultC mv [k'])
    let rk = K.Kind (getSpan $ binder b) (MultVar mv) K.Absorb
--    addConstraint $ KindC (binder b) rk
    return rk
  else
    return k'


isVar :: K.Kind -> Bool
isVar (K.Kind _ MultVar{} K.PKVar{}) = True
isVar (K.Kind _ _ K.PKVar{}) = True
isVar (K.Kind _ MultVar{} _) = True
isVar _ = False
