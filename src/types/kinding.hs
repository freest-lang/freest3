import Types
import qualified Data.Map.Strict as Map

data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show)

data Multiplicity = Un | Lin deriving (Eq, Ord, Show)

data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show)

type Id = String

type Env = Map.Map Id Kind

type Message = String -- move to parser?

kindOf :: Env -> Type -> Either Kind Message
kindOf _ Skip = Left $ Kind Session Un
kindOf _ (Out _) = Left $ Kind Session Lin
kindOf _ (In _) = Left $ Kind Session Lin
kindOf _ (Basic _) = Left $ Kind Arbitrary Un
kindOf delta (Semi t u) =
  case (kindOf delta t, kindOf delta u) of
    (Left(Kind Session m1), Left(Kind Session m2)) ->
      Left $ Kind Session (max m1 m2)
    _                                              ->
      Right $ "One of the operands is not a session kind"
kindOf delta (UnFun t u) =
  case (kindOf delta t, kindOf delta u) of
    (Left(Kind Arbitrary _), Left(Kind Arbitrary _)) ->
      Left $ Kind Arbitrary Lin
    _                                                ->
      Right $ "One of the operands is not a kind of the form T^u. Type: " ++ show (UnFun t u)

-- These should yield Left _
-- kindOf Map.empty (UnFun Skip Skip)
-- kindOf Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kindOf Map.empty (UnFun (In IntType) (In IntType))

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                 = False
