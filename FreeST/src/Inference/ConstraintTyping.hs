{-# LANGUAGE LambdaCase #-}
module Inference.ConstraintTyping (constraintTyping) where

import           Inference.Constraint
import           Inference.ConstraintKinding
import           Inference.Phase
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import qualified Syntax.Type as T
import qualified Typing.Extract as Extract
import           Typing.Rename
import           Typing.Typing
import           Util.Error
import           Util.State

import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map.Strict as Map


type UsageSet = Map.Map Variable K.Kind

constraintTyping :: Variable -> E.Exp -> InfState (T.Type, UsageSet)
constraintTyping f e = do
  (t,uset) <- ctyping Map.empty e
  when (f `Map.member` uset) (
    constraintKinding Map.empty t >>= \(K.Kind s m _) -> unrestricted s m)
  return (t,uset)
  where
    unrestricted :: Span -> Multiplicity -> InfState ()
    unrestricted s (MultVar m) = addConstraint $ MultC m [K.ut s]
    unrestricted _ _           = pure ()
    
ctyping :: K.KindEnv -> E.Exp -> InfState (T.Type, UsageSet)
ctyping _ (E.Int s _) = pure (T.Int s, Map.empty)
ctyping _ (E.Float s _) = pure (T.Float s, Map.empty)
ctyping _ (E.String s _) = pure (T.String s, Map.empty)
ctyping _ (E.Char s _) = pure (T.Char s, Map.empty)
ctyping _ (E.Unit s) = pure (T.unit s, Map.empty)
ctyping kEnv (E.App s fork@(E.Var _ x) e) | x == mkFork s = do
  state <- get 
  (_, _, t2) <- Extract.function e (fst $ evalState (ctyping kEnv e) state)
  put state
  ctyping kEnv (E.App s (E.TypeApp s fork t2) e)
ctyping kEnv (E.App s (E.App _ (E.Var _ x) (E.Var _ c)) e) | x == mkSelect s = do
  (t, u) <- ctyping kEnv e
  m <- Extract.inChoiceMap e t
  t1 <- Extract.choiceBranch s m c t
  return (t1, u)                                         
ctyping kEnv (E.App _ (E.Var s x) e) | x == mkCollect s = do
  (t,u) <- ctyping kEnv e
  tm <- Extract.outChoiceMap e t  
  return (T.Labelled s T.Variant
    (Map.map (T.Labelled s T.Record . Map.singleton (head mkTupleLabels s)) tm), u)
ctyping kEnv (E.App _ (E.Var s x) e) | x == mkReceive s = do
  (t,u) <- ctyping kEnv e
  (t1, t2) <- Extract.input e t
  return (T.tuple s [t1, t2], u)
ctyping kEnv (E.App s (E.App _ (E.Var _ x) e1) e2) | x == mkSend s = do
  (_, u1) <- ctyping kEnv e1
  (t2, u2) <- ctyping kEnv e2
  (_, t4) <- Extract.output e2 t2
  merge u1 u2 
  return (t4, u1 ∪ u2)
ctyping kEnv (E.App _ e1 e2) = do
  (t,u1) <- ctyping kEnv e1
  (_, _, t2) <- Extract.function e1 t
  (_, u2) <- ctyping kEnv e2
  merge u1 u2
  return (t2, u1 ∪ u2)
ctyping kEnv (E.Var p x) = getFromSignatures x >>= \case
  Just t -> constraintKinding kEnv t >>= \k -> return (t, Map.singleton x k)
  Nothing -> addError (TypeVarNotInScope p x) $> (T.Int p, Map.empty)
ctyping kEnv (E.Abs s m b) = do
  k <- constraintKinding kEnv (binder b)
  addToSignatures (var b) (binder b)
  (t, u) <- ctyping kEnv (body b)
  weaken (var b) k u
  when (isAbs (body b)) $ addConstraint $ KindC k (K.Kind s (mult t) K.Top)
  removeFromSignatures (var b)  
  return (T.Arrow s m (binder b) t, Map.delete (var b) u)
  where
    isAbs E.Abs{} = True
    isAbs (E.TypeAbs _ b) = isAbs $ body b
    isAbs _ = False
ctyping kEnv (E.TypeAbs s b) = do
  (t, u) <- ctyping (Map.insert (var b) (binder b) kEnv) (body b)
  return (T.Quant s T.In (Bind s (var b) (binder b) t), u)
ctyping kEnv (E.TypeApp _ e t) = do
  k <- constraintKinding kEnv t
  (t1, u) <- ctyping kEnv e 
  ~(T.Quant _ T.In b) <- Extract.forall e t1
  addConstraint $ KindC k (binder b)
  return (subs t (var b) (body b), u)
ctyping kEnv (E.BinLet _ x y e1 e2) = do
  (t1, u1) <- ctyping kEnv e1
  (t2, t3) <- Extract.pair e1 t1
  addToSignatures x t2 
  addToSignatures y t3
  (t, u2) <- ctyping kEnv e2
  k1 <- constraintKinding kEnv t2
  k2 <- constraintKinding kEnv t3
  merge u1 u2
  mapM_ (\(v, k) -> weaken v k u2) [(x,k1), (y,k2)]
  return (t, Map.delete x (Map.delete y (u1 ∪ u2)))  
ctyping kEnv (E.UnLet _ x e1 e2) = do
  (t, u1) <- ctyping kEnv e1
  addToSignatures x t
  (u, u2) <- ctyping kEnv e2
  k <- constraintKinding kEnv t
  merge u1 u2
  weaken x k (u1 ∪ u2) 
  return (u, Map.delete x (u1 ∪ u2))
ctyping kEnv (E.Case s e m) = do
  (t, u1) <- ctyping kEnv e
  dta <- Extract.datatypeMap e t
  m1 <- buildMap s m dta
  (tl, ul) <- Map.foldr (ctypingMap kEnv) (return ([],[])) m1
  return (head tl, foldr Map.union u1 ul)
ctyping kEnv (E.Pair s e1 e2) = do
  (t, u1) <- ctyping kEnv e1
  (u, u2) <- ctyping kEnv e2 
  let (l0:l1:_) = mkTupleLabels
  let m = Map.insert (l0 defaultSpan) t (Map.singleton (l1 defaultSpan) u)
  merge u1 u2
  return (T.Labelled s T.Record m, u1 ∪ u2)
-- Existentials
ctyping kEnv (E.Pack _ u e2 t) = do
  -- k1 <- constraintKinding kEnv u
  -- (t, u) <- ctyping kEnv e2
  -- k2 <- constraintKinding kEnv t2
  -- ~(T.Quant _ T.Out b) <- Extract.exists e t1
  pure (t, Map.empty)
ctyping kEnv (E.Unpack _ _ x e1 e2) = do
  (t,u1) <- ctyping kEnv e1
  ~(T.Quant _ T.Out (Bind _ b k t12)) <- Extract.exists e1 t
  addToSignatures x t12
  ctyping (Map.insert b k kEnv) e2
ctyping _ e = error $ "undefined: " ++ show e

mult :: T.Type -> Multiplicity
mult (T.Arrow _ m _ _) = m
mult (T.Quant _ _ b) = mult (body b)
mult t = error $ show t    

ctypingMap :: K.KindEnv -> ([Variable], E.Exp) ->
             InfState ([T.Type], [UsageSet]) -> InfState ([T.Type], [UsageSet])
ctypingMap kEnv (xs, e) state = do
  (ts, us) <- state
  (t,u) <- ctyping kEnv e
  return (returnType xs t : ts, u:us)
  where
    returnType :: [Variable] -> T.Type -> T.Type
    returnType [] t                  = t
    returnType (_:xs) (T.Arrow _ _ _ t2) = returnType xs t2
    returnType _ t = t

(∪) :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
(∪) = Map.union

(∩) :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
(∩) u1 u2 = Map.union (Map.intersection u1 u2) (Map.intersection u2 u1)

merge :: UsageSet -> UsageSet -> InfState ()
merge u1 u2 = tMapM_ (\k -> addConstraint (KindC k (K.ut (getSpan k)))) (u1 ∩ u2)

weaken :: Variable -> K.Kind -> UsageSet -> InfState ()
weaken x k sigma
 | Map.member x sigma = return ()
 | otherwise = addConstraint $ KindC k (K.ut (getSpan k))



