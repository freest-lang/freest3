module Types.TypeEquivalence(
  equivalent
, unfold
) where

import Types.Types
import Types.Kinds
import Types.Kinding
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- testar Show
-- testar igualdade rec inside rec and the same for ∀
-- (==) *Types.TypeEquivalence> let t1 = read "(forall x . !Int;x);forall y . !Bool;y" :: Type
-- *Types.TypeEquivalence> let t2 = read "(forall y . !Int;y);forall x . !Bool;x" :: Type
-- sem parens

-- Type bisimulation

data Label = OutLabel BasicType |
             InLabel BasicType |
             ExtChoiceLabel TypeVar |
             IntChoiceLabel TypeVar |
             VarLabel TypeVar
             deriving (Eq, Ord, Show)

terminated :: Type -> Bool
terminated Skip = True
terminated (Semi t1 t2) = terminated t1 && terminated t2
terminated t
    | isRec t = terminated $ unfold t
    | otherwise = False

reduce :: Type -> Map.Map Label Type
reduce (Var x)            = Map.singleton (VarLabel x) Skip
reduce (Out b)            = Map.singleton (OutLabel b) Skip
reduce (In b)             = Map.singleton (InLabel b) Skip
reduce (Semi t1 t2)
    | terminated t1       = reduce t2
    | otherwise           = Map.map (\t -> if t == Skip then t2 else t `Semi` t2) (reduce t1)
reduce (Choice Internal m) = Map.mapKeys IntChoiceLabel m
reduce (Choice External m) = Map.mapKeys ExtChoiceLabel m
reduce (Rec x t)          = reduce $ unfold (Rec x t)
reduce _                  = Map.empty

--TODO: equiv' Forall Forall
equivalent :: Type -> Type -> Bool
equivalent = equiv Set.empty

equiv :: Set.Set(Type,Type) -> Type -> Type -> Bool
equiv s t1 t2
    | (t1,t2) `Set.member` s = True
    | otherwise              = equiv' s t1 t2

equiv' _ (Var x) (Var y)                = x == y
equiv' _ (Basic b) (Basic c)            = b == c
equiv' s (Fun m1 t1 t2) (Fun m2 t3 t4)    = m1 == m2 && equiv s  t1 t3 && equiv s t2 t4
equiv' s (PairType t1 t2) (PairType t3 t4)      = equiv s t1 t3 && equiv s t2 t4
equiv' s (Datatype dt1) (Datatype dt2)  = Map.size dt1 == Map.size dt2 &&
      Map.foldlWithKey (\b l t -> b && l `Map.member` dt2 &&
                        equiv s (dt2 Map.! l) t) True dt1
-- equiv s (Forall x t1) (Forall y t2) = equiv s t1 t2
equiv' s t1 t2
    | isRec t1             = equiv (Set.insert (t1, t2) s) (unfold t1) t2
    | isRec t2             = equiv (Set.insert (t1, t2) s) t1 (unfold t2)
    | isSessionType t1 &&
        isSessionType t2   = Map.size r1 == Map.size r2 &&
                              Map.foldlWithKey (\b l t -> b && Map.member l r2 &&
                                equiv s t (r2 Map.! l))  True r1
    | otherwise = False
      where r1 = reduce t1
            r2 = reduce t2

isRec :: Type -> Bool
isRec (Rec _ _) = True
isRec _         = False

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec x t) = subs (Rec x t) x t

-- Assume the second type is closed (no free vars)
subs :: Type -> TypeVar -> Type -> Type
subs _ _ Skip               = Skip
subs _ _ (In b)             = In b
subs _ _ (Out b)            = Out b
subs t y (Var x)
    | x == y                = t
    | otherwise             = Var x
subs t y (Semi t1 t2)       = Semi (subs t y t1) (subs t y t2)
subs t y (PairType t1 t2)   = PairType (subs t y t1) (subs t y t2)
subs t2 y (Forall x t1)
    | x == y                = Forall x t1
    | otherwise             = Forall x (subs t2 y t1)
subs t2 y (Rec x t1)
    | x == y                = Rec x t1
    | otherwise             = Rec x (subs t2 y t1)
subs t y (Choice v m) = Choice v (Map.map(subs t y) m)
