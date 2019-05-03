module Compiler (compileFile) where

import           CodeGen.CodeGen (genProgram)
import           Control.Monad.State
import qualified Data.Set as Set
import           Parse.Parser (parseProgram)
import           Syntax.Expressions (ExpEnv)
import           Syntax.Schemes (TypeEnv, VarEnv)
import           System.Exit
import           System.FilePath
import           Utils.FreestState
import           Utils.PreludeLoader (prelude)
import           Validation.Rename (renameState)
import           Validation.TypeChecking (typeCheck)


compileFile :: FilePath -> IO ()
compileFile args 
  | "fst" `isExtensionOf` args = do
      s1 <- parseProgram args prelude
      let s2 = execState renameState s1
      let s3 = execState typeCheck s2
      genCode (errors s3) (varEnv s3) (expEnv s3) (typeEnv s3) args
  | otherwise = die $ "Error: File extension not recognized, provide a .fst file: " ++ args


-- CODE GEN
genCode :: Errors -> VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO ()
genCode err venv eenv cenv path
  | Set.null err  = do
      genProgram venv eenv cenv path
      -- TODO: codeGen is turned off for now
      -- compileAndRun path
      exitSuccess -- remove 
  | otherwise = die $ getErrors err
    -- printErrors err >> exitFailure
    -- putStrLn ("\n\n" ++ show err ++ "\n\n" ) >> exitFailure
  -- TODO: pretty print errors

-- compileAndRun :: String -> IO ()
-- compileAndRun filepath = do
--   let (path, filename) = splitFileName filepath
--   changeDir path
--   (exitcode, _, errors) <- readProcessWithExitCode "ghc" [targetFileName filename] ""
--   checkGhcOut exitcode errors
    
--   (exitcode1, output1, errors1) <- readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
--   checkGhcOut exitcode1 errors1
--   putStr output1
--   exitSuccess
      

-- changeDir :: String -> IO ()
-- changeDir d
--   | null d    = return ()
--   | otherwise = setCurrentDirectory d

-- targetFileName :: String -> String
-- targetFileName file = replaceExtensions file "hs"

-- checkGhcOut :: ExitCode -> String -> IO ()
-- checkGhcOut ExitSuccess _ = return ()
-- checkGhcOut _ errors = putStrLn errors >> exitFailure
