module Compiler (compile) where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Terms.Parser
import           Terms.Terms
import           Types.Types
import           TypeChecking.TypeChecking
import           Control.Monad.Writer
import           Data.List
import           System.Exit

-- import           System.Process


compile :: String -> IO Bool
compile arg = do
  
  -- putStrLn "Starting parser ...\n"

  -- curDir <- getCurrentDirectory
  -- prog <- mainProgram (curDir ++ "/src/test.hs") prelude
  prog <- mainProgram arg prelude

  (venv, eenv, tenv, cenv) <-
    case prog of
      Left err -> do
        putStr (show err)
        return $ error "Parser Error"
      Right d -> return d

  -- putStrLn "No parser errors found... \n"
  -- putStrLn "Type Checking...\n"
  
  let tc = Map.mapWithKey (\fun (a, e) -> typeCheck venv tenv a e fun) eenv
    --  datatypeGen
    --  typeGen
  if all (== True) (Map.map typeChecks tc) then
    codeGen tenv venv eenv
  else
    printErr tc


showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e = f ++ (showParams as) ++ " = " ++ show e ++ "\n\n"

showType :: String -> TypeVar -> Type -> String
showType acc tv t = acc ++ "type " ++ tv ++ " = " ++ show t ++ "\n\n"
  
typeChecks :: TCheckM Type -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM Type -> [String]
showErrors = snd . runWriter

codeGen :: TypeEnv -> VarEnv -> ExpEnv -> IO Bool 
codeGen tenv venv eenv = do 
  let types = Map.foldlWithKey showType "" tenv
  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (venv Map.! fun))
                           ++ showExpr fun a e) "main = putStrLn (show start)\n\n" eenv
  writeFile "cfst.hs" (types ++ file)
  -- callCommand "ghc cfst.hs"
  -- callCommand "./cfst"
  return True

printErr :: Map.Map TermVar (TCheckM Type)-> IO Bool
printErr tc = do
--putStrLn "Errors \n"   
  let err = Map.foldl (\acc v -> acc ++ showErrors v) [] tc
  putStrLn $ intercalate "\n" err  
  return False
