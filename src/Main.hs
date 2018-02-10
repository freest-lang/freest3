module Main where

import Terms.Parser


main :: IO ()
main = do
  putStrLn "Starting parser ...\n"
  ; prog     <- mainProgram
  ; prog     <- case prog of
                 Left err -> do{ putStr (show(err)); return $ error ""}
                 Right d  -> return d
  ; print prog
  -- ; putStrLn ""
