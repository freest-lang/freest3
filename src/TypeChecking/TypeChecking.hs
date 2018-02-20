{-# OPTIONS_GHC -fno-warn-tabs #-}
module TypeChecking.TypeChecking () where

import Types.Kinds
import Types.Kinding
import Types.Types
import Types.TypeEquivalence
import Terms.Terms
-- import Terms.Parser
import qualified Data.Map.Strict as Map
import System.Log.Logger

{-
--TODO: remove
--type KindEnv = Map.Map Id Kind
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
-}

-- TODO: ERROR
-- The function ‘a’ is applied to two arguments,
--    but its type ‘Int’ has none
{-
typeCheckFunction :: String -> (TypeEnv,ExpEnv) -> TypeCheckOut
typeCheckFunction funName (tEnv,eEnv) =

		if ((length types)-1 == (length args)) then
			typeCheck Map.empty tEnv (argsMap types args) (tEnv Map.! funName) (snd(eEnv Map.! funName))
		else
			Right $ "not equal number of args " ++ funName ++ "\n (length types)= " ++ show (length types -1) ++ " | (length args) = " ++  show (length args)

		where
				types = deconstructType (tEnv Map.! funName)
				args  = fst(eEnv Map.! funName)
-}
type ArgsMap = Map.Map String Type

argsMap :: [Type] -> Args -> ArgsMap
argsMap t xs = Map.fromList $ zip xs t


-- TODO: cant be like this?  right associativity
deconstructType :: Type -> [Type]
deconstructType (Fun _ t1 t2) = [t1] ++ deconstructType t2
deconstructType t = [t]
{-
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
-}

typeCheck :: VarEnv -> Expression -> IO(Type)
typeCheck venv exp = do
  debugM "Type Checking" ("Goal: " ++ (show venv) ++ " |- " ++ (show exp))
  (t, venv) <- checkExp venv exp
--  checkVEnvUn venv -- TODO
  debugM "Type Checking" "Done!"
  return t

checkExp :: VarEnv -> Expression -> IO(Type, VarEnv)
-- Basic expressions
checkExp venv Unit = return (Basic UnitType, venv)
checkExp venv (Integer _) = return (Basic IntType, venv)
checkExp venv (Character _) = return (Basic CharType, venv)
checkExp venv (Boolean _) = return (Basic BoolType, venv)
-- Variables
checkExp venv (Variable x) = return (venv Map.! x, venv)
-- Aplication
checkExp venv1 (Application e1 e2) = do
   (t1, venv2) <- checkExp venv1 e1
   (t2, t3) <- checkFun t1
   (t4, venv3) <- checkExp venv2 e2
   checkEquivTypes t2 t4
   return (t3, venv3)
-- Conditional
checkExp venv1 (Conditional e1 e2 e3) = do
  (t1, venv2) <- checkExp venv1 e1
  checkBool t1
  (t2, venv3) <- checkExp venv2 e2
  (t3, venv4) <- checkExp venv3 e3
  checkEquivTypes t2 t3
  checkEquivEnvs  venv3 venv4
  return (t2, venv3)
-- Pairs
checkExp venv1 (Pair e1 e2) = do
  (t1, venv2) <- checkExp venv1 e1
  (t2, venv3) <- checkExp venv2 e2
  return (PairType t1 t2, venv3)
checkExp venv1 (Let x1 x2 e1 e2) = do
  (t1, venv2) <- checkExp venv1 e1
  (t2, t3) <- checkPair t1
  (t4, venv3) <- checkExp (Map.insert x2 t3 (Map.insert x1 t2 venv2)) e2
  return (t4, venv3)
-- Session types
checkExp venv (New t) = do
  t <- checkSessionType t
  return (PairType t (dual t), venv)
{-
checkExp venv1 (Send e1 e2) = (Fun Un b (Fun Un t1 t2), venv3)
  where (b, venv2) = checkExp venv1 e1
        (t1, venv3) = checkExp venv2 e2
  where _ = if isSessionType t2 then () else error "New type is not session type"
-}

checkEquivTypes :: Type -> Type -> IO()
checkEquivTypes t1 t2
  | equivalent t1 t2 = return ()
  | otherwise        = errorM "Type Checking" ("Expecting type " ++ (show t1) ++ " to be equivalent to type " ++ (show t2))

checkEquivEnvs :: VarEnv -> VarEnv -> IO()
checkEquivEnvs venv1 venv2
  | equivalentEnvs  venv1 venv2 = return ()
  | otherwise                   = errorM "Type Checking"
      ("Expecting enviroment " ++ (show venv1) ++ " to be equivalent to enviroment " ++ (show venv2))

checkFun :: Type -> IO (Type, Type)
checkFun (Fun _ t1 t2) = return (t1, t2)
checkFun t             = do
  errorM "Type Checking" ("Expecting a function type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkPair :: Type -> IO (Type, Type)
checkPair (PairType t1 t2) = return (t1, t2)
checkPair t             = do
  errorM "Type Checking" ("Expecting a pair type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkSessionType :: Type -> IO(Type)
checkSessionType t
  | isSessionType t = return t
  | otherwise       = do
      errorM "Type Checking" ("Expecting a session type; found " ++ (show t))
      return Skip

checkBool :: Type -> IO ()
checkBool (Basic BoolType) = return ()
checkBool t                = errorM "Type Checking" ("Expecting a boolean type; found " ++ (show t))

{-
-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file
checkExpEnv :: VarEnv -> ExpEnv -> Bool
checkExpEnv venv eenv = Map.foldrWithKey (\fun pair b -> b && checkFun venv fun pair) True eenv

checkFun venv1 fun (args, exp) = checkExp venv2 exp
  where venv2 = venv1 -- TODO: add venv1 fun args
-}

-- TODO: mapas para o fim

-- Variable environments

equivalentEnvs :: VarEnv -> VarEnv -> Bool
equivalentEnvs _ _ = True -- TODO: fix me!

-- Type environments

checkTypeEnv :: TypeEnv -> Bool
checkTypeEnv tenv = Map.foldr (\(_,t) b -> b && isType kindEnv t) True tenv
  where kindEnv = Map.map fst tenv

