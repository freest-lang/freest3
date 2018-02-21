module Types.Kinding
( isType
, isSessionType
, isSchemeType
, kindOf
, contractive
, Kind (..)
, KindEnv
, un) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types
import           Data.Either (lefts,rights)
import           Data.List (intercalate)

type KindEnv = Map.Map TypeVar Kind
type Message = String
type KindingOut = Either Kind Message

isType :: KindEnv -> Type -> Bool
isType kenv t = case kinding kenv t of
  Left _  -> True
  Right _ -> False

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                = False

isSessionType :: Type -> Bool
isSessionType = isSession . kindOf

isScheme :: Kind -> Bool
isScheme (Kind Scheme _) = True
isScheme _               = False

isSchemeType :: Type -> Bool
isSchemeType = isScheme . kindOf

un :: Type -> Bool
un = isUn . kindOf

isUn :: Kind -> Bool
isUn (Kind _ Un) = True
isUn (Kind _ _) = False

kindOf :: Type -> Kind
kindOf t = case kinding Map.empty t of
  Left k  -> k
  Right m -> error $ "Type " ++ show t ++ " not a proper type:\n" ++ m
  -- Right _ -> error $ "Type " ++ show t ++ " not a proper type"

kinding :: KindEnv -> Type -> KindingOut
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
kinding delta (Fun m t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2) |  k1 <= Kind Arbitrary Lin && k2 <= Kind Arbitrary Lin ->
      Left $ Kind Arbitrary m
    _                                                                          ->
      Right $ "One of the operands is a type Scheme. Type: " ++ show (Fun m t u)
kinding delta (PairType t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2 ) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin) ->
        Left $ Kind Arbitrary Lin
    _                                                                               ->
      Right $ "One of the operands is a type Scheme. Type: " ++ show (PairType t u)
kinding delta (Datatype m) =
  kindingDatatypeMap delta m (Kind Arbitrary Un)
    ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m)
kinding delta (Choice _ m) =
  kindingMap delta m (Kind Session Lin)
    ("One of the components in a choice isn't lower than a S^l. \nType: " ++ show m)
kinding delta (Rec x t) =
  let km = kinding  (Map.insert x (Kind Session Un) delta) t in
  case km of
    (Left k) ->
     if contractive (Map.insert x k delta) t
        then
          if (k <= (Kind Arbitrary Lin))
            then Left k
            else Right $ "The kind of the type is a type Scheme. \nType: " ++ show (Rec x t)
        else Right $ "The body of the type is not contractive. \nType: " ++ show (Rec x t)
    (Right m) -> Right m
kinding delta (Forall x t) =
  let kd = kinding (Map.insert x (Kind Session Un) delta) t in
  case kd of
    -- TODO: k is the kinding of the variable and it is always Kind Session Un ?
    -- (Left k') | k' >= (Kind Scheme Un) -> Left k'
    (Left k') | k' <= (Kind Arbitrary Lin) -> Left k'
    (Right m) -> Right m
    -- _ -> Right "Forall body is not a type Scheme"
kinding delta (Var x) =
  if Map.member x delta then
    Left $ delta Map.! x
  else
    Right $ show x ++ " is a free variable"


kindingMap :: KindEnv -> TypeMap -> Kind -> Message -> KindingOut
kindingMap delta m k message =
  let km = liftl $ map (kinding delta) (Map.elems m) in
  case km of
    (Left ks) ->
      if all (<= k) ks then
        Left $ (Kind Session Lin)
      else
        Right message
    (Right ms) -> Right $ intercalate "\n" ms

kindingDatatypeMap :: KindEnv -> TypeMap -> Kind -> Message -> KindingOut
kindingDatatypeMap delta m k message =
  let km = liftl $ map (kinding delta) (Map.elems m) in
  case km of
    (Left ks) ->
      if all (<= k) ks then
        Left $ Kind Arbitrary (multiplicity $ maximum ks)
      else
        Right message
    (Right ms) -> Right $ intercalate "\n" ms

-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))

multiplicity :: Kind -> Multiplicity
multiplicity (Kind _ m) = m

liftl :: [KindingOut] -> Either [Kind] [Message]
liftl xs =
  let a = rights xs in
  if length a == 0
    then
      Left $ lefts xs
    else
      Right a

-- Contractivity
contractive :: KindEnv -> Type -> Bool
contractive delta (Semi t _) = contractive delta t
contractive delta (Rec _ t) = contractive delta t
contractive delta (Var x) = Map.member x delta
contractive delta (Forall _ t) = contractive delta t
contractive _ _ = True

-- (int -> int);skip -> malformed

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))
