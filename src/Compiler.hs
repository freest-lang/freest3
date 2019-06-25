module Compiler (compileFile) where

import           System.FilePath

compileFile :: FilePath -> IO ()
compileFile _ = putStrLn "COMPILER"
  -- | "fst" `isExtensionOf` args = do
  --     s1 <- parseProgram args prelude
  --     let s2 = execState renameState s1
  --     let s3 = execState typeCheck s2
  --     if hasErrors s3
  --     then
  --       die $ getErrors s3
  --     else
  --       genCode (varEnv s3) (expEnv s3) (typeEnv s3) args
  -- | otherwise = die $ "Error: File extension not recognized, provide a .fst file: " ++ args
