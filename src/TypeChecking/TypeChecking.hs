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
		-- a <- pure $ typeCheck Map.empty prog (UnBoolApp "not" (BasicTerm BoolType))
		putStrLn "\n"
		print prog
		putStrLn "\n"
		a <- pure $ Map.mapWithKey (\k y -> typeCheckFunction k prog) (snd prog)
		mapM_ print a
		return ()

-- TODO: ERROR
-- The function ‘a’ is applied to two arguments,
--    but its type ‘Int’ has none
typeCheckFunction :: String -> (TypeEnv,ExpEnv) -> TypeCheckOut
typeCheckFunction funName (tEnv,eEnv) =

		if ((length types)-1 == (length args)) then
			-- Right $ "equal number of args " ++ funName ++ "\n (length types)= " ++ show (length types -1) ++ " | (length args) = " ++  show (length args)
			typeCheck Map.empty (tEnv,eEnv) (argsMap types args) (snd(eEnv Map.! funName))
		else
			Right $ "not equal number of args " ++ funName ++ "\n (length types)= " ++ show (length types -1) ++ " | (length args) = " ++  show (length args)

		where
				types = deconstructType (tEnv Map.! funName)
				args  = fst(eEnv Map.! funName)

-- tf :: Id -> TypeEnv -> Args -> Map.Map String Type
-- tf id tEnv  = argsMap (tEnv Map.! id)

argsMap :: [Type] -> Args -> Map.Map String Type
argsMap t xs = Map.fromList $ zip xs t

-- argsMap :: Type -> Args -> Map.Map String Type
-- argsMap t xs = Map.fromList $ zip xs (deconstructType t)


-- TODO: cant be like this
deconstructType :: Type -> [Type]
deconstructType (UnFun t1 t2) = [t1] ++ deconstructType t2
deconstructType (LinFun t1 t2) = [t1] ++ deconstructType t2
deconstructType t = [t]


typeCheck :: KindEnv -> (TypeEnv,ExpEnv) -> Map.Map String Type -> Expression -> TypeCheckOut
typeCheck _ (m1,_) args (BasicTerm b) =
		if un (Basic b) then
			Left $ ((Basic b), m1)
		else
			Right $ "Type isn't unrestricted: " ++ show b
typeCheck delta (m1,m2) args (IntApp op e1 e2) =
		case (typeCheck delta (m1,m2) args e1, typeCheck delta (m1,m2) args e2) of
			(Left (Basic IntType, _), Left (Basic IntType, _)) 		->
				Left $ (Basic IntType, m1)
			_ 																										->
				Right $ "One of the operands is not of an Int type: " ++ show e1 ++ " | " ++ show e2
typeCheck delta (m1,m2) args (BoolApp op e1 e2) =
		case (typeCheck delta (m1,m2) args e1, typeCheck delta (m1,m2) args e2) of
			(Left (Basic BoolType, _), Left (Basic BoolType, _)) 		->
				Left $ (Basic BoolType, m1)
			_ 																										->
				Right $ "One of the operands is not of a Bool type: " ++ show e1 ++ " | " ++ show e2
typeCheck delta (m1,m2) args (UnBoolApp op e1) =
		case (typeCheck delta (m1,m2) args e1) of
			(Left (Basic BoolType, _)) 		->
				Left $ (Basic BoolType, m1)
			_ 																										->
				Right $ "The operand is not of a Bool type: " ++ show e1
typeCheck delta (m1,m2) args (Terms.Terms.Var x) =
	if Map.member x args then
		Left $ ((args Map.! x), m1)
	else
		Right $ "Not in scope '" ++ show x ++ "'"
