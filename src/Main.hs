module Main where

import Terms.Parser
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
  putStrLn "Starting parser ...\n"
  prelude <- mainProgram "src/Terms/prelude.hs" Map.empty
  prelude <- case prelude of
               Left err -> do{ putStr (show(err)); return $ error ""}
               Right d  -> return d
  -- putStrLn "\n"
  -- print prelude
  -- putStrLn "\n"
  -- prog     <- mainProgram "src/Terms/test.hs" Map.empty
  -- prog     <- case prog of
  --                Left err -> do{ putStr (show(err)); return $ error ""}
  --                Right d  -> return d
  -- print prog
  -- ; putStrLn ""
