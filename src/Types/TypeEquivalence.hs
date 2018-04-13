module Types.TypeEquivalence(
  equivalent
, unfold
, subs  
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
             ChoiceLabel ChoiceView TypeVar |
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
reduce (Choice v m) = Map.mapKeys (ChoiceLabel v) m
--reduce (Choice External m) = Map.mapKeys ExtChoiceLabel m
reduce (Rec x k t)          = reduce $ unfold (Rec x k t)
reduce _                  = Map.empty

--TODO: equiv' Forall Forall
equivalent :: KindEnv -> Type -> Type -> Bool
equivalent = equiv Set.empty

equiv :: Set.Set(Type,Type) -> KindEnv -> Type -> Type -> Bool
equiv s kenv t1 t2
    | (t1,t2) `Set.member` s = True
    | otherwise              = equiv' s kenv t1 t2

equiv' :: Set.Set(Type,Type) -> KindEnv -> Type -> Type -> Bool
equiv' _ _ (Var x) (Var y)                = x == y
equiv' _ _ (Basic b) (Basic c)            = b == c

equiv' s kenv (Fun m1 t1 t2) (Fun m2 t3 t4)  =
  m1 == m2 && (equiv s kenv t1 t3) && (equiv s kenv t2 t4)
  
equiv' s kenv (PairType t1 t2) (PairType t3 t4) = equiv s kenv t1 t3 && equiv s kenv t2 t4
   
equiv' s kenv (Datatype dt1) (Datatype dt2) =
  (Map.size dt1 == Map.size dt2) && (Map.foldlWithKey (checkBinding kenv dt2 s) True dt1)
-- -- equiv s (Forall x t1) (Forall y t2) = equiv s t1 t2

equiv' s kenv (Rec x k t1) t2 = equiv (Set.insert ((Rec x k t1), t2) s) kenv (unfold (Rec x k t1)) t2
equiv' s kenv t1 (Rec x k t2) = equiv (Set.insert (t1, (Rec x k t2)) s) kenv t1 (unfold (Rec x k t2))

equiv' s kenv t1 t2 = do 
  -- isSessionType t1 && isSessionType t2
  -- equivElems <- Map.foldlWithKey (checkBinding r2 s) (return True) r1
  -- return $ Map.size r1 == Map.size r2 && equivElems
  equivSessionTypes s kenv (isSessionType kenv t1 && isSessionType kenv t2) t1 t2

-- equiv' _ _ _ = return False
equivSessionTypes :: Set.Set (Type, Type) -> KindEnv -> Bool -> Type -> Type -> Bool 
equivSessionTypes s kenv st t1 t2 
  | st        = Map.size r1 == Map.size r2 && Map.foldlWithKey (checkBinding kenv r2 s) True r1      
  | otherwise = False
  where r1 = reduce t1
        r2 = reduce t2
        
--checkBinding :: TypeMap -> Set.Set (Type, Type) -> IO Bool -> TypeVar -> Type -> IO Bool
checkBinding kenv tm s acc l t = acc && l `Map.member` tm && (checkEquivVar s kenv tm l t) 

--checkVar :: Set.Set(Type,Type) -> TypeMap -> TypeVar -> Type -> IO Bool
checkEquivVar s kenv tm l t
  | Map.member l tm = equiv s kenv (tm Map.! l) t
  | otherwise       = False

  


-- equiv' s t1 t2
--     | isRec t1             = equiv (Set.insert (t1, t2) s) (unfold t1) t2
--     | isRec t2             = equiv (Set.insert (t1, t2) s) t1 (unfold t2)
--     | isSessionType t1 &&
--         isSessionType t2   = Map.size r1 == Map.size r2 &&
--                               Map.foldlWithKey (\b l t -> b && Map.member l r2 &&
--                                 equiv s t (r2 Map.! l))  True r1
--     | otherwise = False
--       where r1 = reduce t1
--             r2 = reduce t2


  

isRec :: Type -> Bool
isRec (Rec _ _ _) = True
isRec _         = False

-- Assumes parameter is a Rec type
unfold :: Type -> Type
unfold (Rec x k t) = subs (Rec x k t) x t

subs :: Type -> TypeVar -> Type -> Type
subs t y (Var x)
    | x == y                = t
    | otherwise             = Var x
subs t y (Semi t1 t2)       = Semi (subs t y t1) (subs t y t2)
subs t y (PairType t1 t2)   = PairType (subs t y t1) (subs t y t2)
-- subs t2 y (Forall x k t1)
--     | x == y                = Forall x k t1
--     | otherwise             = Forall x k (subs t2 y t1)
-- Assume y /= x 
subs t2 y (Rec x k t1)
    | x == y                = Rec x k t1
    | otherwise             = Rec x k (subs t2 y t1)
subs t y (Choice v m)       = Choice v (Map.map(subs t y) m)
subs t y (Fun m t1 t2)      = Fun m (subs t y t1) (subs t y t2)
subs _ _ t                  = t
-- subs _ _ Skip               = Skip
-- subs _ _ (In b)             = In b
-- subs _ _ (Out b)            = Out b
