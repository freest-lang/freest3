{-# LANGUAGE BlockArguments #-}
module Inference.Inference (infer) where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( isDatatypeContructor )
import qualified Syntax.Type as T
import qualified Kinding.Subkind as SK (join)
import           Kinding.Kinding (synthetise)
import           Util.State

import           Inference.ConstraintKinding
import           Inference.ConstraintTyping
import           Inference.Unification
import           Inference.Phase

import           Data.Bifunctor
import           Data.Either
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map

-- | Kind
infer :: InfState ()
infer = do
  getTypes >>= tMapM_ (constraintKinding Map.empty . snd)
  s <- getSignatures
  getSignatures >>= tMapM_ (constraintKinding Map.empty)
  getDefs >>= tMapWithKeyM_ constraintTyping
  setSignatures s
  subs <- unify
  getTypes >>= setTypes . Map.map (bimap (subsKind subs) (subsOnType subs))
  getSignatures >>= setSignatures . Map.map (subsOnType subs)
  getDefs >>= setDefs . Map.map (subsOnExp subs)
  fixConsTypes


-- TODO: Type Class
subsKind :: Substitution -> K.Kind -> K.Kind
subsKind subs (K.Kind s (MultVar x) (K.PKVar y)) =
  K.Kind s (fromRight Lin $ subs Map.! x) (fromLeft K.Top $ subs Map.! y) 
subsKind subs (K.Kind s (MultVar y) pk) = K.Kind s (fromRight Lin $ subs Map.! y) pk
subsKind subs (K.Kind s m (K.PKVar y)) = K.Kind s m (fromLeft K.Top $ subs Map.! y)
subsKind _ k = k  


subsOnType :: Substitution -> T.Type -> T.Type
subsOnType subs (T.Arrow s m t1 t2) = T.Arrow s m (subsOnType subs t1) (subsOnType subs t2)
subsOnType subs (T.Labelled s sort m) = T.Labelled s sort (Map.map (subsOnType subs) m)
subsOnType subs (T.Semi s t1 t2) = T.Semi s (subsOnType subs t1) (subsOnType subs t2)
subsOnType subs (T.Message s p t) = T.Message s p (subsOnType subs t)
subsOnType subs (T.Forall s b) = T.Forall s (subsOnTBind subs b)
subsOnType subs (T.Rec s b) = T.Rec s (subsOnTBind subs b)
subsOnType subs (T.Dualof s t) = T.Dualof s (subsOnType subs t)
subsOnType _ t = t


subsOnTBind :: Substitution -> Bind K.Kind T.Type -> Bind K.Kind T.Type
subsOnTBind subs (Bind s x k t) = Bind s x (subsKind subs k) (subsOnType subs t)

subsOnEBind :: Substitution -> Bind T.Type E.Exp -> Bind T.Type E.Exp
subsOnEBind subs (Bind s x t e) = Bind s x (subsOnType subs t) (subsOnExp subs e)

subsOnKEBind :: Substitution -> Bind K.Kind E.Exp -> Bind K.Kind E.Exp
subsOnKEBind subs (Bind s x t e) = Bind s x (subsKind subs t) (subsOnExp subs e)


subsOnExp :: Substitution -> E.Exp -> E.Exp
subsOnExp subs (E.Abs s m b) = E.Abs s m (subsOnEBind subs b)
subsOnExp subs (E.App s e1 e2) = E.App s (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.Pair s e1 e2) = E.Pair s (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.BinLet s x y e1 e2) = E.BinLet s x y (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.Case    s e fm) = E.Case s e (Map.map (second (subsOnExp subs)) fm)
subsOnExp subs (E.TypeAbs s b) = E.TypeAbs s (subsOnKEBind subs b) 
subsOnExp subs (E.TypeApp s e t) = E.TypeApp s (subsOnExp subs e) (subsOnType subs t) 
subsOnExp subs (E.UnLet s x e1 e2) = E.UnLet s x (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp _ e = e


-- | Fix the multiplicity of the data constructor types
-- | TODO: Later these should be inferred
fixConsTypes :: InfState ()
fixConsTypes = do
  tys <- getTypes
  -- if this is the first step in the elaboration, there are still type names in
  -- signatures, so we need a non-empty kind environment. Empty env otherwise.
  let kEnv = Map.map fst tys
  getSignatures >>= tMapWithKeyM_ \k v -> S.when (isDatatypeContructor k tys)
    (fixConsType kEnv K.Un v >>= addToSignatures k)
  where
    fixConsType :: K.KindEnv -> K.Multiplicity -> T.Type -> InfState T.Type
    fixConsType kEnv m (T.Arrow s _ t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      T.Arrow s m t <$> fixConsType kEnv (SK.join m m') u
    fixConsType _ _ t = pure t
