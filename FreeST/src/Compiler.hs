module Compiler (compileFile) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Interpreter.Builtin
import           Interpreter.Eval
import           Interpreter.Value
import           Parse.Parser (parseProgram)
import           Syntax.Base
import           Syntax.Expressions (ExpEnv)
import           Syntax.Schemes (TypeEnv, VarEnv)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO (stdout)
import           System.Process
import           Utils.FreestState
import           Utils.PreludeLoader (prelude)
import           Validation.BuildTypes
import           Validation.Rename (renameState)
import           Validation.TypeChecking (typeCheck)

compileFile :: FilePath -> IO ()
compileFile args
  | "fst" `isExtensionOf` args = do
      -- Parsing
      s1 <- parseProgram args prelude
      when (hasErrors s1) (die $ getErrors s1)
      -- Renaming the state
      let s2 = execState renameState s1
      -- Solving type declarations and dualofs
      let s3 = execState solveTypeDecls s2
      when (hasErrors s3) (die $ getErrors s3)
      -- TypeChecking
      let s4 = execState typeCheck s3
      when (hasErrors s4) (die $ getErrors s4)
      -- Interpreter
      res <- eval initialCtx (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
      case res of
        IOValue io -> io >>= putStrLn . show
        _          -> putStrLn $ show res
      return ()
  | otherwise = die $ "Error: File extension not recognized, provide a .fst file: " ++ args
