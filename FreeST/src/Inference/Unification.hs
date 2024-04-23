module Inference.Unification where

import           Inference.Phase
import           Kinding.Subkind
import           Syntax.Base
import           Syntax.Constraint
import qualified Syntax.Kind as K
import           Util.Error
import           Util.State

import           Data.Either
import           Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State hiding (join)


type Substitution = Map.Map Variable (Either K.PreKind Multiplicity)

unify :: InfState Substitution 
unify = do
  mVars <- getMVariables
  pkVars <- getPKVariables
  cset <- getConstraints
  flip isValid cset =<< unify' (initialSubs mVars pkVars) cset
  where
    unify' sigma cset = unifyInner sigma cset >>= \(sigma', cset') ->
      if sigma == sigma' then pure sigma else unify' sigma' cset'

initialSubs :: Set.Set Variable -> Set.Set Variable -> Substitution
initialSubs mVars pkVars = Set.foldl (\acc mvar -> Map.insert mvar (Right topMult) acc)
  (Set.foldl (\acc pkvar -> Map.insert pkvar (Left topPK) acc) Map.empty pkVars) mVars
  where 
    topMult = Lin
    topPK   = K.Top

isValid :: Substitution -> [Constraint] -> InfState Substitution
isValid sigma cset = mapM_ isConstraintValid cset $> sigma
  where
    isConstraintValid :: Constraint -> InfState ()
    isConstraintValid (KindC k1 k2)  =
         unless (fromSigma sigma k1 <: fromSigma sigma k2) $
          addError (CantUnifyKind (getSpan k1) (fromSigma sigma k1) (fromSigma sigma k2))
    isConstraintValid _ = pure ()

unifyInner :: Substitution -> [Constraint] -> InfState (Substitution, [Constraint])
unifyInner sigma [] = pure (sigma, [])
unifyInner sigma (c@(KindC k1 k2):cs) =
  unifyInner (joinSubstitutions sigma (substitution sigma k1 k2)) cs >>= \(s, cset) -> pure (s,c:cset)
unifyInner sigma (c@(MultC m ms):cs) =
  let subs = Map.singleton m (Right $ foldl (\acc k -> join (fromRight Un (mult sigma k)) acc) Un ms) in
  unifyInner (joinSubstitutions sigma subs) cs >>= \(s, cset) -> pure (s,c:cset)
unifyInner sigma (c@(PKMeetC pk pks):cs) =
  let subs = Map.singleton pk (Left $ foldl (\acc k -> meet (fromLeft K.Top (prekind sigma k)) acc) K.Top pks) in
  unifyInner (joinSubstitutions sigma subs) cs >>= \(s, cset) -> pure (s,c:cset)
unifyInner sigma (c@(PKJoinC pk pks):cs) =
  let subs = Map.singleton pk (Left $ foldl (\acc k -> join (fromLeft K.Absorb (prekind sigma k)) acc) K.Absorb pks) in
  unifyInner (joinSubstitutions sigma subs) cs >>= \(s, cset) -> pure (s,c:cset)

fromSigma :: Substitution -> K.Kind -> K.Kind
fromSigma sigma (K.Kind s (MultVar m) (K.PKVar pk)) = K.Kind s (fromRight K.Un (sigma Map.! m)) (fromLeft K.Top (sigma Map.! pk))
fromSigma sigma (K.Kind s (MultVar m) pk) = K.Kind s (fromRight K.Un (sigma Map.! m)) pk
fromSigma sigma (K.Kind s m (K.PKVar pk)) = K.Kind s m (fromLeft K.Top (sigma Map.! pk))
fromSigma _ k = k

-- \circ in the paper
joinSubstitutions :: Substitution -> Substitution -> Substitution
joinSubstitutions = Map.unionWith liftMeet
  where
    liftMeet (Left m1) (Left m2) = Left $ meet m1 m2 
    liftMeet (Right pk1) (Right pk2) = Right $ meet pk1 pk2
    liftMeet e1 e2 = error $ "joinSubstitutions.liftMeet: (" ++ show e1 ++ ") (" ++ show e2 ++ ")"

substitution :: Substitution -> K.Kind -> K.Kind -> Substitution
substitution sigma (K.Kind _ (MultVar m) (K.PKVar pk)) k2 = Map.fromList [(m, mult sigma k2), (pk, prekind sigma k2)]
substitution sigma (K.Kind _ (MultVar m) _) k2 = Map.fromList [(m, mult sigma k2)]
substitution sigma (K.Kind _ _ (K.PKVar pk)) k2 = Map.fromList [(pk, prekind sigma k2)]
substitution _ _ _ = Map.empty

mult :: Substitution -> K.Kind -> Either K.PreKind Multiplicity
mult sigma (K.Kind _ (MultVar m) _) = Right . fromRight K.Un $ sigma Map.! m
mult _ (K.Kind _ m _) = Right m


prekind :: Substitution -> K.Kind -> Either K.PreKind Multiplicity
prekind sigma (K.Kind _ _ (K.PKVar pk)) = Left . fromLeft K.Top $ sigma Map.! pk
prekind _ (K.Kind _ _ pk) = Left pk
