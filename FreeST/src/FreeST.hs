module FreeST (checkAndRun) where

import System.Environment (getArgs)
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Interpreter.Builtin
import           Interpreter.Eval (evalAndPrint)
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

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    let filePath = head args in
    if "fst" `isExtensionOf` filePath
    then checkAndRun filePath
    else die $ "Error: File extension not recognized, provide a .fst file: " ++ filePath
  else
    putStrLn "Error: Incorrect number of arguments, provide just one argument"

checkAndRun :: FilePath -> IO ()
checkAndRun filePath = do
  -- Parse
  s1 <- parseProgram filePath prelude
  when (hasErrors s1) (die $ getErrors s1)
  -- Rename
  let s2 = execState renameState s1
  -- Solve type declarations and dualofs
  let s3 = execState solveTypeDecls s2
  when (hasErrors s3) (die $ getErrors s3)
  -- Type check
  let s4 = execState typeCheck s3
  when (hasErrors s4) (die $ getErrors s4)
  -- Interpret
  evalAndPrint initialCtx (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
