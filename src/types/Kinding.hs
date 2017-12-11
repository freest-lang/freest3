module Kinding
( isType
, kindOf) where

import Types
import qualified Data.Map.Strict as Map

data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show)

data Multiplicity = Un | Lin deriving (Eq, Ord, Show)

data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show)

type Env = Map.Map Id Kind

type Message = String -- move to parser?

isType :: Type -> Bool
isType t = case kinding Map.empty t of
  Left _  -> True
  Right _ -> False

kindOf :: Type -> Kind
kindOf t = case kinding Map.empty t of
  Left k  -> k
  Right m -> error $ "Type " ++ show t ++ " not a proper type:\n" ++ m
  -- Right _ -> error $ "Type " ++ show t ++ " not a proper type"

kinding :: Env -> Type -> Either Kind Message
kinding _ Skip = Left $ Kind Session Un
kinding _ (Out _) = Left $ Kind Session Lin
kinding _ (In _) = Left $ Kind Session Lin
kinding _ (Basic _) = Left $ Kind Arbitrary Un
kinding delta (Semi t u) =
  case (kinding delta t, kinding delta u) of
    (Left(Kind Session m1), Left(Kind Session m2))  ->
      Left $ Kind Session (max m1 m2)
    _                                              ->
      Right $ "One of the operands is not a session kind"
kinding delta (UnFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2) |  k1 <= Kind Arbitrary Lin && k2 <= Kind Arbitrary Lin          ->
      Left $ Kind Arbitrary Un
    _                                                                                         ->
      Right $ "Error Message. Type: " ++ show (UnFun t u)
kinding delta (LinFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left(k1), Left(k2)) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin)          ->
      Left $ Kind Arbitrary Lin
    _                                                                                         ->
      Right $ "Error Message. Type: " ++ show (LinFun t u)
kinding delta (Pair t u) =
  case (kinding delta t, kinding delta u) of
    (Left(k1), Left(k2)) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin)          ->
      Left $ Kind Arbitrary Lin
    _                                                                                         ->
      Right $ "Error Message. Type: " ++ show (Pair t u)
kinding delta (Datatype xs) =
  --  [Left (Kind Arbitrary Un),Left (Kind Arbitrary Un)]
  let a = map (kinding delta) (allTypes (Map.toList xs))
  in
    if (all (isLeftKind) a)
      then Left $ Kind Arbitrary (minimum $ map allMultiplicities a)
      else Right $ "Error Message. Type: " ++ show (Datatype xs)
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
kinding delta (InternalChoice xs) =
  kindOfIntAndExtChoices delta xs
kinding delta (ExternalChoice xs) =
  kindOfIntAndExtChoices delta xs
kinding delta (Rec x k) =
    let k' = kinding delta k
    in
      if(test k')
        then Left $ t1 k'
        else Right $ "Error Message. Type: "

-- <= (Kind Arbitrary Lin) or Multiplicity lower than lin

kindOfIntAndExtChoices delta xs =
  let a = map (kinding delta) (allTypes (Map.toList xs))
  in
    if (all (isLeftSession) a)
      then Left $ Kind Session Lin
      else Right $ "One of the operands is not a kind of the form T^u. \n " ++ show a

allTypes :: [(a,b)] -> [b]
allTypes = map (\(_,b) -> b)

allMultiplicities :: Either Kind b -> Multiplicity
allMultiplicities x =
  case x of
    (Left (Kind _ m1)) ->
      m1
      -- TODO: look for exhaustive patterns here

isLeftSession :: Either Kind b -> Bool
isLeftSession x =
  case x of
    (Left (Kind Session _))   ->
      True
    _                         ->
      False

isLeftKind :: Either Kind b -> Bool
isLeftKind x =
  case x of
    (Left (Kind v1 _))  | v1 <= Arbitrary  ->
      True
    _                         ->
      False

t1 x =
  case x of
    (Left (k1)) ->
      k1


test x =
  case x of
    (Left (k1)) | k1 <= (Kind Arbitrary Lin) ->
      True
    _                                        ->
      False

contractive :: Env -> Type -> Bool
contractive _ (UnFun _ _) = True
contractive _ (LinFun _ _) = True
contractive _ (Pair _ _) = True
contractive _ (Datatype _) = True
contractive delta (Semi t _) = contractive delta t
contractive delta (Rec _ t) = contractive delta t
contractive delta (Var x) = Map.member x delta


-- (int -> int);skip -> malformed

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                 = False
