module Inference.Unification where

import           Syntax.Base
import           Syntax.Constraint
import qualified Syntax.Kind as K
import           Inference.Phase
import           Kinding.Subkind


import           Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Substitution = Map.Map Variable (Either K.PreKind Multiplicity)

unify :: InfState Substitution 
unify = do
  mVars <- getMVariables
  pkVars <- getPKVariables
  cset <- getConstraints
  unify' (initialSubs mVars pkVars) cset
  where
    unify' sigma cset = unifyInner sigma cset >>= \sigma' ->
      if sigma == sigma' then pure sigma else unify' sigma' cset

initialSubs :: Set.Set Variable -> Set.Set Variable -> Substitution
initialSubs mVars pkVars = Set.foldl (\acc mvar -> Map.insert mvar (Right topMult) acc)
                             (Set.foldl (\acc pkvar -> Map.insert pkvar (Left topPK) acc) Map.empty pkVars)
                           mVars
 where 
  topMult = Lin
  topPK   = K.Top




  
unifyInner :: Substitution -> [Constraint] -> InfState Substitution
unifyInner sigma [] = pure sigma
unifyInner sigma (KindC (K.Kind _ (MultVar m) (K.PKVar pk)) k2:cs) =
  let subs = Map.fromList [(m, mult sigma k2), (pk, prekind sigma k2)] in
  unifyInner (joinSubstitutions sigma subs) cs

  
-- TODO: check!!! There are more cases ?? 
unifyInner sigma (KindC (K.Kind _ (MultVar m) _) k2:cs) =
  unifyInner (joinSubstitutions sigma (Map.singleton m (mult sigma k2))) cs
unifyInner sigma (KindC (K.Kind _ _ (K.PKVar pk)) k2:cs) =
  unifyInner (joinSubstitutions sigma (Map.singleton pk (prekind sigma k2))) cs

unifyInner sigma (KindC k1 (K.Kind _ (MultVar m) (K.PKVar pk)):cs) =
  -- let subs = Map.fromList [(m, mult sigma k1), (pk, prekind sigma k1)] in
  -- unifyInner (joinSubstitutions sigma subs) cs
  unifyInner sigma cs
unifyInner sigma (KindC k1 (K.Kind _ (MultVar m) _):cs) = unifyInner sigma cs
--  unifyInner (joinSubstitutions sigma (Map.singleton m (mult sigma k1))) cs
unifyInner sigma (KindC k1 (K.Kind _ _ (K.PKVar pk)):cs) = unifyInner sigma cs
--  unifyInner (joinSubstitutions sigma (Map.singleton pk (prekind sigma k1))) cs

unifyInner sigma c@(KindC k1 k2: cs) = -- unifyInner sigma cs
  if k1 <: k2 then unifyInner sigma cs
  else error $  "cant unify:" ++ show (getSpan k1) ++ ": " ++ show k1 ++ " <: " ++ show k2 
  
unifyInner sigma (MultC m ms:cs) =
  let subs = Map.singleton m (Right $ foldl (\acc k -> join (fromRight Un (mult sigma k)) acc) Un ms) in
  unifyInner (joinSubstitutions sigma subs) cs
unifyInner sigma (PKMeetC pk pks:cs) =
  let subs = Map.singleton pk (Left $ foldl (\acc k -> meet (fromLeft K.Top (prekind sigma k)) acc) K.Top pks) in
  unifyInner (joinSubstitutions sigma subs) cs
unifyInner sigma (PKJoinC pk pks:cs) =
  let subs = Map.singleton pk (Left $ foldl (\acc k -> join (fromLeft K.Absorb (prekind sigma k)) acc) K.Absorb pks) in
  unifyInner (joinSubstitutions sigma subs) cs


joinSubstitutions :: Substitution -> Substitution -> Substitution
joinSubstitutions = Map.unionWith liftMeet
  where
    liftMeet (Left m1) (Left m2) = Left $ meet m1 m2 
    liftMeet (Right pk1) (Right pk2) = Right $ meet pk1 pk2
    liftMeet e1 e2 = error $ "joinSubstitutions.liftMeet: (" ++ show e1 ++ ") (" ++ show e2 ++ ")"

mult :: Substitution -> K.Kind -> Either K.PreKind Multiplicity
mult sigma (K.Kind _ (MultVar m) _) = Right . fromRight K.Un $ sigma Map.! m
mult _ (K.Kind _ m _) = Right m


prekind :: Substitution -> K.Kind -> Either K.PreKind Multiplicity
prekind sigma (K.Kind _ _ (K.PKVar pk)) = Left . fromLeft K.Top $ sigma Map.! pk
prekind _ (K.Kind _ _ pk) = Left pk
