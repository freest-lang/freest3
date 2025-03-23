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
  getTypes >>= setTypes . Map.map (bimap (substitute subs) (substitute subs))
  getSignatures >>= setSignatures . Map.map (substitute subs)
  getDefs >>= setDefs . Map.map (substitute subs)
  fixConsTypes


class KindSubs a where
  substitute :: Substitution -> a -> a

instance KindSubs K.Kind where
  substitute subs (K.Kind s (MultVar x) (K.PKVar y)) =
    K.Kind s (fromRight Lin $ subs Map.! x) (fromLeft K.Top $ subs Map.! y) 
  substitute subs (K.Kind s (MultVar y) pk) = K.Kind s (fromRight Lin $ subs Map.! y) pk
  substitute subs (K.Kind s m (K.PKVar y)) = K.Kind s m (fromLeft K.Top $ subs Map.! y)
  substitute _ k = k   

instance KindSubs T.Type where
  substitute subs (T.Arrow s m l1 l2 t1 t2) = T.Arrow s m l1 l2 (substitute subs t1) (substitute subs t2)
  substitute subs (T.Labelled s sort m) = T.Labelled s sort (Map.map (substitute subs) m)
  substitute subs (T.Semi s t1 t2) = T.Semi s (substitute subs t1) (substitute subs t2)
  substitute subs (T.Message s l p t) = T.Message s l p (substitute subs t)
  substitute subs (T.Forall s b) = T.Forall s (substitute subs b)
  substitute subs (T.Rec s b) = T.Rec s (substitute subs b)
  substitute subs (T.Dualof s t) = T.Dualof s (substitute subs t)
  substitute _ t = t

instance (KindSubs a, KindSubs b) => KindSubs (Bind a b) where
  substitute subs (Bind s x k t) = Bind s x (substitute subs k) (substitute subs t)

instance KindSubs E.Exp where
  substitute subs (E.Abs s m b) = E.Abs s m (substitute subs b)
  substitute subs (E.App s e1 e2) = E.App s (substitute subs e1) (substitute subs e2)
  substitute subs (E.Pair s e1 e2) = E.Pair s (substitute subs e1) (substitute subs e2)
  substitute subs (E.BinLet s x y e1 e2) = E.BinLet s x y (substitute subs e1) (substitute subs e2)
  substitute subs (E.Case    s e fm) = E.Case s e (Map.map (second (substitute subs)) fm)
  substitute subs (E.TypeAbs s b) = E.TypeAbs s (substitute subs b) 
  substitute subs (E.TypeApp s e t) = E.TypeApp s (substitute subs e) (substitute subs t) 
  substitute subs (E.UnLet s x e1 e2) = E.UnLet s x (substitute subs e1) (substitute subs e2)
  substitute _ e = e


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
    fixConsType kEnv m (T.Arrow s _ l1 l2 t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      T.Arrow s m l1 l2 t <$> fixConsType kEnv (SK.join m m') u
    fixConsType _ _ t = pure t
