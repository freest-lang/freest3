{-# OPTIONS_GHC -fno-warn-tabs #-}
module TypeChecking.TypeChecking () where

import Terms.Terms
import Types.Types
import Types.Kinding
import Terms.Parser
import qualified Data.Map.Strict as Map

--TODO: remove
type KindEnv = Map.Map Id Kind
type Message = String
type TypeCheckOut = Either (Type,TypeEnv) Message

test = do
		prog <- mainProgram "src/Terms/test.hs"
		prog     <- case prog of
									 Left err -> do{ putStr (show(err)); return $ error ""}
									 Right d  -> return d
		a <- pure $ typeCheck Map.empty prog (UnBoolApp (BasicTerm IntType))
		print a
		return ()

typeCheck :: KindEnv -> (TypeEnv,ExpEnv) -> Expression -> TypeCheckOut
typeCheck _ (m1,_) (BasicTerm b) =
		if un (Basic b) then
			Left $ ((Basic b), m1)
		else
			Right $ "Type isn't unrestricted: " ++ show b
typeCheck delta (m1,m2) (IntApp e1 e2) =
		case (typeCheck delta (m1,m2) e1, typeCheck delta (m1,m2) e2) of
			(Left (Basic IntType, _), Left (Basic IntType, _)) 		->
				Left $ (Basic IntType, m1)
			_ 																										->
				Right $ "One of the operands is not of an Int type: " ++ show e1 ++ " | " ++ show e2
typeCheck delta (m1,m2) (BoolApp e1 e2) =
		case (typeCheck delta (m1,m2) e1, typeCheck delta (m1,m2) e2) of
			(Left (Basic BoolType, _), Left (Basic BoolType, _)) 		->
				Left $ (Basic BoolType, m1)
			_ 																										->
				Right $ "One of the operands is not of a Bool type: " ++ show e1 ++ " | " ++ show e2
typeCheck delta (m1,m2) (UnBoolApp e1) =
		case (typeCheck delta (m1,m2) e1) of
			(Left (Basic BoolType, _)) 		->
				Left $ (Basic BoolType, m1)
			_ 																										->
				Right $ "The operand is not of a Bool type: " ++ show e1
