module Main where

import qualified Data.Map.Strict as Map
import           Terms.Parser
import           TypeChecking.TypeChecking
import           Data.List
import           System.Directory
import           System.Environment

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  putStrLn "Starting parser ...\n"
  prelude <- mainProgram (curDir ++ "/src/prelude.hs") Map.empty
  (prelude, _, _) <-
    case prelude of
      Left err -> do
        putStr (show err)
        return $ error ""
      Right d -> return d
  -- putStrLn "\n"
  -- print $ prelude
  -- putStrLn "\n"
  args <- getArgs
  prog <- mainProgram (curDir ++ "/src/test.hs") prelude
  --prog <- mainProgram (head args) prelude
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
  let a = Map.mapWithKey (\fun (a, e) -> typeCheck p1 p3 a e fun) p2
--  mapM (>>= putStrLn . show) a

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

showExpr f as e = f ++ (showArgs as) ++ " = " ++ show e ++ "\n\n"

showArgs as
  | null as = ""
  | otherwise = " " ++ (intercalate " " as)
