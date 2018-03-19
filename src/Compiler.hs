module Main where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           System.Environment
import           Terms.Parser
import           Terms.Terms
import           Types.Types
import           TypeChecking.TypeChecking
import           Control.Monad.Writer
import           Data.List


main :: IO ()
main = do
  
  putStrLn "Starting parser ...\n"

  args <- getArgs
  -- curDir <- getCurrentDirectory
  -- prog <- mainProgram (curDir ++ "/src/test.hs") prelude
  prog <- mainProgram (head args) prelude

  (venv, eenv, tenv, cenv) <-
    case prog of
      Left err -> do
        putStr (show err)
        return $ error ""
      Right d -> return d

  -- h <- fileHandler "debug.log" ERROR >>= \lh -> return $ 
  --             setFormatter lh (simpleLogFormatter "$msg")
  -- updateGlobalLogger "TypeChecking" (setLevel ERROR . addHandler h) 
  

  putStrLn "No parser errors found... \n"
  putStrLn "Type Checking...\n"
  
  --Map.foldlWithKey (\acc fun (a, e) -> typeCheck venv tenv a e fun) (return ()) eenv
--  pure $ Map.mapWithKey (\fun (a, e) -> test venv tenv a e fun) eenv
  let tc = Map.mapWithKey (\fun (a, e) -> typeCheck venv tenv a e fun) eenv
  -- let b = Map.foldlWithKey (\acc fun (a, e) -> acc && typeChecks (typeCheck venv tenv a e fun)) True eenv
--  mapM (>>= putStrLn . show) a

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

codeGen :: TypeEnv -> VarEnv -> ExpEnv -> IO () 
codeGen tenv venv eenv = do 
  putStrLn "Linking... \n"
  let types = Map.foldlWithKey showType "" tenv
  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (venv Map.! fun))
                           ++ showExpr fun a e) "main = putStrLn (show start)\n\n" eenv
  writeFile "cfst.hs" (types ++ file)
  putStrLn "Linking... \n"
  return ()

printErr :: Map.Map TermVar (TCheckM Type)-> IO ()
printErr tc = do
 putStrLn "Errors \n"
 let err = Map.foldl (\acc v -> acc ++ showErrors v) [] tc
 putStrLn $ intercalate "\n" err
 return ()
