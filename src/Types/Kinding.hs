module Types.Kinding
( isType
, isSessionType
, kindOf
, contractive
, Kind (..)) where
  
import Types.Types
import qualified Data.Map.Strict as Map
import Data.Either as E
import Data.List

data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show, Read)
data Multiplicity = Un | Lin deriving (Eq, Ord, Show, Read)
data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show, Read)

type Env = Map.Map Id Kind
type Message = String
type KindingOut = Either Kind Message

isType :: Type -> Bool
isType t = case kinding Map.empty t of
  Left _  -> True
  Right _ -> False

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                 = False

isSessionType :: Type -> Bool
isSessionType  = isSession . kindOf


kindOf :: Type -> Kind
kindOf t = case kinding Map.empty t of
  Left k  -> k
  Right m -> error $ "Type " ++ show t ++ " not a proper type:\n" ++ m
  -- Right _ -> error $ "Type " ++ show t ++ " not a proper type"

kinding :: Env -> Type -> KindingOut
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
    (Left k1, Left k2) |  k1 <= Kind Arbitrary Lin && k2 <= Kind Arbitrary Lin      ->
      Left $ Kind Arbitrary Un
    _                                                                               ->
      Right $ "One of the operands is a type Scheme. Type: " ++ show (UnFun t u)
kinding delta (LinFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin)  ->
      Left $ Kind Arbitrary Lin
    _                                                                               ->
      Right $ "One of the operands is a type Scheme. Type: " ++ show (LinFun t u)
kinding delta (Pair t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2 ) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin) ->
        Left $ Kind Arbitrary Lin
    _                                                                               ->
      Right $ "One of the operands is a type Scheme. Type: " ++ show (Pair t u)
kinding delta (Datatype m) =
  kindingMap delta m (Kind Arbitrary Un)
    ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m)
kinding delta (ExternalChoice m) =
  kindingMap delta m (Kind Session Lin)
    ("One of the components in an ExternalChoice isn't lower than a S^l. \nType: " ++ show m)
kinding delta (InternalChoice m) =
  kindingMap delta m (Kind Session Lin)
    ("One of the components in an InternalChoice isn't lower than a S^l. \nType: " ++ show m)
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
    (Left k) | k <= (Kind Arbitrary Lin) -> Left k
    (Right m) -> Right m
kinding delta (Var x) =
  if Map.member x delta then
    Left $ delta Map.! x
  else
    Right $ show x ++ " is a free variable"

kindingMap :: Env -> TypeMap -> Kind -> Message -> KindingOut
kindingMap delta m k message =
  let km = liftl $ map (kinding delta) (Map.elems m) in
  case km of
    (Left ks) ->
      if all (<= k) ks then
        Left $ maximum ks
      else
        Right message
    (Right ms) -> Right $ intercalate "\n" ms


-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))

liftl :: [KindingOut] -> Either [Kind] [Message]
liftl xs =
  let a = rights xs in
  if length a == 0
    then
      Left $ lefts xs
    else
      Right a

-- Contractivity
contractive :: Env -> Type -> Bool
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
