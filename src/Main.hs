
module Main where

import qualified Data.Map.Strict as Map
import           Terms.Parser
import           TypeChecking.TypeChecking
import           Data.List


main :: IO ()
main = do
  putStrLn "Starting parser ...\n"
  prelude <- mainProgram "src/prelude.hs" Map.empty
  (prelude, _, _) <-
    case prelude of
      Left err -> do
        putStr (show err)
        return $ error ""
      Right d -> return d
  -- putStrLn "\n"
  -- print $ prelude
  -- putStrLn "\n"
  prog <- mainProgram "src/test.hs" prelude
  (p1, p2, p3) <-
    case prog of
      Left err -> do
        putStr (show err)
        return $ error ""
      Right d -> return d

  -- printEnv p1 "VarEnv"
  -- printEnv p2 "ExpEnv"
  -- printEnv p3 "TypeEnv"
  -- printEnv p4 "ConstructorEnv"
  putStrLn "No parser errors found... \n"
  putStrLn "TypeChecking...\n"
  let a = Map.mapWithKey (\fun (a, e) -> typeCheck a e fun p1 p3) p2
  mapM (>>= putStrLn . show) a

  putStrLn "Linking... \n"

  let b = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (p1 Map.! fun))
                           ++ showExpr fun a e) "" p2
  putStrLn b
  
  
  return ()

--type ParserOut = (VarEnv, ExpEnv, TypeEnv, ConstructorEnv)
-- typeCheck :: Expression -> VarEnv -> IO(Type)
printEnv p desc = do
  putStrLn $ desc ++ "\n"
  print p
  putStrLn ""
  
 

showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f a e = f ++ " " ++ (intercalate " " a) ++ " = " ++ show e ++ "\n\n"
