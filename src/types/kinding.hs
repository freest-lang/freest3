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
  Right _ -> error $ "Type " ++ show t ++ "not a proper type"

kinding :: Env -> Type -> Either Kind Message
kinding _ Skip = Left $ Kind Session Un
kinding _ (Out _) = Left $ Kind Session Lin
kinding _ (In _) = Left $ Kind Session Lin
kinding _ (Basic _) = Left $ Kind Arbitrary Un
kinding delta (Semi t u) =
  case (kinding delta t, kinding delta u) of
    (Left(Kind Session m1), Left(Kind Session m2)) ->
      Left $ Kind Session (max m1 m2)
    _                                              ->
      Right $ "One of the operands is not a session kind"
kinding delta (UnFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left(Kind Arbitrary _), Left(Kind Arbitrary _)) ->
      Left $ Kind Arbitrary Lin
    _                                                ->
      Right $ "One of the operands is not a kind of the form T^u. Type: " ++ show (UnFun t u)

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                 = False
