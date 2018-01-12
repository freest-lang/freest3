module Types.TypeEquivalence() where

import Types.Types
import Types.Kinding
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types.Parser
--TODO: Remove import

-- testar Show
-- testar igualdade rec inside rec and the same for ∀
-- (==) *Types.TypeEquivalence> let t1 = read "(forall x . !Int;x);forall y . !Bool;y" :: Type
-- *Types.TypeEquivalence> let t2 = read "(forall y . !Int;y);forall x . !Bool;x" :: Type
-- sem parens

-- Type bissimulation

data Label = OutLabel BasicType |
             InLabel BasicType |
             ExtChoiceLabel Field |
             IntChoiceLabel Field |
             VarLabel Id
             deriving (Eq,Ord,Show)

-- substitute :: Env -> Type -> Type

terminated :: Type -> Bool
terminated Skip = True
terminated (Semi t1 t2) = terminated t1 && terminated t2
terminated _ = False

reduce :: Type -> Map.Map Label Type
reduce (Var x) = Map.singleton (VarLabel x) Skip
reduce (Out b) = Map.singleton (OutLabel b) Skip
reduce (In b)  = Map.singleton (InLabel b) Skip
reduce (Semi t1 t2)
      | terminated t1 = reduce t2
      | otherwise =  Map.map (`Semi` t2) (reduce t1)
reduce (InternalChoice m) = Map.mapKeys IntChoiceLabel m
reduce (ExternalChoice m) = Map.mapKeys ExtChoiceLabel m
reduce _ = Map.empty
-- reduce t t
--   | terminated t = Map.fromList[(t, Skip)]
--   | otherwise = error $ show t

equivalent :: Type -> Type -> Bool
equivalent = equiv Set.empty

equiv :: Set.Set(Type,Type) -> Type -> Type -> Bool
-- equiv _ Skip Skip = True
equiv _ (Var x) (Var y) = x == y
equiv _ (Basic b) (Basic c) = b == c
equiv s (UnFun t1 t2) (UnFun t3 t4) = equiv s t1 t3 && equiv s t2 t4
equiv s (LinFun t1 t2) (LinFun t3 t4) = equiv s t1 t3 && equiv s t2 t4
equiv s (Pair t1 t2) (Pair t3 t4) = equiv s t1 t3 && equiv s t2 t4
equiv s (Datatype dt1) (Datatype dt2) =
    Map.foldlWithKey (\b l t -> b && l `Map.member` dt2 &&
                      equiv s (dt2 Map.! l) t) True dt1
equiv s t1 t2
  | isSessionType t1 && isSessionType t2  =
      Map.foldlWithKey (\b l t -> b && Map.member l r2 &&
              equiv (Set.insert (t1,t2) s) (r2 Map.! l) t)  True r1
  | otherwise = False
    where r1 = reduce t1
          r2 = reduce t2

--TODO: equiv Rec Rec
--TODO: equiv Forall Forall


-- equivalent :: Type -> Type -> Bool
-- equivalent t1 t2 = equiv Set.empty (unfold t1) (unfold t2)
--
-- -- assume that both types are unfolded
-- equiv :: Set.Set(Type,Type) -> Type -> Type -> Bool
-- -- equiv _ Skip Skip = True
-- equiv _ (Var x) (Var y) = x == y
-- equiv _ (Basic b) (Basic c) = b == c
-- equiv s (UnFun t1 t2) (UnFun t3 t4) = equiv s (unfold t1) (unfold t3) &&
--                                       equiv s (unfold t2) (unfold t4)
-- equiv s (LinFun t1 t2) (LinFun t3 t4) = equiv s (unfold t1) (unfold t3) &&
--                                         equiv s (unfold t2) (unfoldt4)
-- equiv s (Pair t1 t2) (Pair t3 t4) = equiv s (unfold t1) (unfold t3) &&
--                                     equiv s (unfold t2) (unfold t4)
-- equiv s (Datatype dt1) (Datatype dt2) =
--     Map.foldlWithKey (\b l t -> b && l `Map.member` dt2 &&
--                       equiv s (unfold (dt2 Map.! l)) (unfold t)) True dt1
-- equiv s t1 t2
--   | isSessionType t1 && isSessionType t2  =
--       Map.foldlWithKey (\b l t -> b && Map.member l r2 &&
--               equiv (Set.insert (t1,t2) s) (unfold (r2 Map.! l)) (unfold t))  True r1
--   | otherwise = False
--     where r1 = reduce t1
--           r2 = reduce t2
--
-- unfold :: Type -> Type
-- unfold (Rec x t) = unfold(subs (Rec x t) t x))
-- unfold t = t
