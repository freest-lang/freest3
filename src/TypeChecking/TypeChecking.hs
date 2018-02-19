{-# OPTIONS_GHC -fno-warn-tabs #-}
module TypeChecking.TypeChecking () where

import Types.Kinds
import Types.Kinding
import Types.Types
import Terms.Terms
import Terms.Parser
import qualified Data.Map.Strict as Map

--TODO: remove
type KindEnv = Map.Map Id Kind
type Message = String
type TypeCheckOut = Either (Type,TypeEnv) Message


test = do
		prelude <- mainProgram "src/Terms/prelude.hs" Map.empty
		prelude <- case prelude of
								 Left err -> do{ putStr (show(err)); return $ error ""}
								 Right d  -> return d
		-- putStrLn "\n"
 		-- print prelude
 		-- putStrLn "\n"
		prog <- mainProgram "src/Terms/test.hs" (fst prelude)
		prog <- case prog of
							 Left err -> do{ putStr (show(err)); return $ error ""}
							 Right d  -> return d
		-- a <- pure $ typeCheck Map.empty prog (UnBoolApp "not" (BasicTerm BoolType))
		-- putStrLn "\n"
		-- print prog
		-- putStrLn "\n"
		file <- pure $ Map.mapWithKey (\k y -> typeCheckFunction k prog) (snd prog)
		print file
		return ()


-- TODO: ERROR
-- The function ‘a’ is applied to two arguments,
--    but its type ‘Int’ has none
typeCheckFunction :: String -> (TypeEnv,ExpEnv) -> TypeCheckOut
typeCheckFunction funName (tEnv,eEnv) =

		if ((length types)-1 == (length args)) then
			typeCheck Map.empty tEnv (argsMap types args) (tEnv Map.! funName) (snd(eEnv Map.! funName))
		else
			Right $ "not equal number of args " ++ funName ++ "\n (length types)= " ++ show (length types -1) ++ " | (length args) = " ++  show (length args)

		where
				types = deconstructType (tEnv Map.! funName)
				args  = fst(eEnv Map.! funName)

argsMap :: [Type] -> Args -> ArgsMap
argsMap t xs = Map.fromList $ zip xs t


-- TODO: cant be like this?  right associativity
deconstructType :: Type -> [Type]
deconstructType (Fun _ t1 t2) = [t1] ++ deconstructType t2
deconstructType t = [t]

type ArgsMap = Map.Map String Type
typeCheck :: KindEnv -> TypeEnv -> ArgsMap -> Type -> Expression -> TypeCheckOut
typeCheck _ m1 arg _ (BasicTerm b) =
	--TODO: change un predicate
		if un (Basic b) then
			Left $ ((Basic b), m1)
		else
			Right $ "Type isn't unrestricted: " ++ show b
typeCheck delta m1 args t (App op e1 e2) =
		case (typeCheck delta m1 args t e1, typeCheck delta m1 args t e2) of
			(Left (t1, _), Left (t2, _)) 		->
				-- TODO: get the left operator type and compare with t1 (same for t2)
				-- error $ (show t) ++ "\n" ++ show (deconstructType (m1 Map.! op))
				let (x:y:z) = deconstructType (m1 Map.! op) in
				 	if (x == t1 && y == t2)		 then
				 		Left $ ((head z), m1)
					else
						throwErr x y t1 t2
			_ 																										->
				Right $ "One of the operands is not a valid type: " ++ show e1 ++ " | " ++ show e2
typeCheck delta m1 args _ (Terms.Terms.Var x) =
		if Map.member x args then
			Left $ ((args Map.! x), m1)
		else
			Right $ "Not in scope '" ++ show x ++ "'"
typeCheck delta m1 args t (ExpPair e1 e2) =
	case (typeCheck delta m1 args t e1, typeCheck delta m1 args t e2) of
		(Left (t1, _), Left (t2, _)) 		->
			-- Left $ (Pair t1 t2 , m1)
			cmpReturnType (Pair t1 t2) (head (reverse (deconstructType t))) m1
		_ 																										->
			Right $ "One of the operands is not a valid type: " ++ show e1 ++ " | " ++ show e2

typeCheck _ _ _ _ t = error $ show t -- TODO : remove after adding all patterns

cmpReturnType t1 t2 m1 =
	if(t1 == t2) then
		Left (t1, m1)
	else
		Right $ "Couldn't match expected type '" ++ show t2 ++ "' with actual type '" ++ show t1 ++ "'"

throwErr x y t1 t2 =
	if x /= t1 then
		Right $ "Couldn't match expected type '" ++ show x ++ "' with actual type '" ++ show t1 ++ "'"
	else
		Right $ "Couldn't match expected type '" ++ show y ++ "' with actual type '" ++ show t2 ++ "'"
