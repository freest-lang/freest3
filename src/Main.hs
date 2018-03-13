module Main where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           System.Directory
import           System.Environment
import           Terms.Parser
import           Terms.Terms
import           TypeChecking.TypeChecking

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.IO

main :: IO ()
main = do
  
  putStrLn "Starting parser ...\n"

  args <- getArgs
  curDir <- getCurrentDirectory
  prog <- mainProgram (curDir ++ "/src/test.hs") prelude
--  prog <- mainProgram (head args) prelude
  (p1, p2, p3) <-
    case prog of
      Left err -> do
        putStr (show err)
        return $ error ""
      Right d -> return d

  -- s <- openlog "TypeChecking" [PID] USER ERROR
  -- updateGlobalLogger rootLoggerName (addHandler s)
  -- updateGlobalLogger "TypeChecking" (setLevel ERROR)
  -- printEnv p1 "VarEnv"
  -- printEnv p2 "ExpEnv"
  -- printEnv p3 "TypeEnv"
  -- printEnv p4 "ConstructorEnv"
  putStrLn "No parser errors found... \n"
  putStrLn "Type Checking...\n"

  
--  errorM "TypeChecking" "errroo"
  
  let a = Map.mapWithKey (\fun (a, e) -> typeCheck p1 p3 a e fun) p2
  mapM (>>= putStrLn . show) a
  
  -- h <- streamHandler stdout ERROR >>= \lh -> return $ 
  --             setFormatter lh (simpleLogFormatter "$msg")
  -- updateGlobalLogger "TypeChecking" (addHandler h)            


  

  putStrLn "Linking... \n"

  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (p1 Map.! fun))
                           ++ showExpr fun a e) "main = putStrLn (show start)\n" p2
  --  putStrLn (b++c)

  writeFile "cfst.hs" file
  return ()

  -- printEnv p desc = do
--   putStrLn $ desc ++ "\n"
--   print p
--   putStrLn ""



showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e = f ++ (showParams as) ++ " = " ++ show e ++ "\n\n"

