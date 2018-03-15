module Main where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           System.Environment
import           Terms.Parser
import           Terms.Terms
import           Types.Types
import           TypeChecking.TypeChecking

-- import System.Log.Logger
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple
-- import System.Log.Handler (setFormatter, close)
-- import System.Log.Formatter
-- import System.IO


main :: IO ()
main = do
  
  putStrLn "Starting parser ...\n"

  args <- getArgs
  --curDir <- getCurrentDirectory
  --prog <- mainProgram (curDir ++ "/src/test.hs") prelude
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
  let a = Map.mapWithKey (\fun (a, e) -> typeCheck venv tenv a e fun) eenv
  mapM (>>= putStrLn . show) a

  --close h

  putStrLn "Linking... \n"

--  datatypeGen
--  typeGen
  let types = Map.foldlWithKey showType "" tenv

  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (venv Map.! fun))
                           ++ showExpr fun a e) "main = putStrLn (show start)\n\n" eenv
  
  writeFile "cfst.hs" (types ++ file)
  return ()


showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e = f ++ (showParams as) ++ " = " ++ show e ++ "\n\n"

showType :: String -> TypeVar -> Type -> String
showType acc tv t = acc ++ "type " ++ tv ++ " = " ++ show t ++ "\n\n"
  

