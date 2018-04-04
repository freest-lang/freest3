module Compiler (compile) where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Terms.Parser
import           Terms.Terms
import           Types.Types
import           TypeChecking.TypeChecking
import           Control.Monad.Writer
import           Data.List
import           System.Exit
import           Types.Kinds

-- import           System.Process


compile :: String -> IO (Bool, String)
compile arg = do
  
  prog <- mainProgram arg prelude

  (venv, eenv, tenv, cenv, kenv) <-
    case prog of
      Left err -> do
        putStr (show err)
        return $ error "Parser Error"
      Right d -> return d

  let tc = Map.mapWithKey (\fun (a, e) -> typeCheck kenv venv cenv a e fun) eenv
    --  datatypeGen
    --  typeGen
  if all (== True) (Map.map typeChecks tc) then
    codeGen tenv venv eenv (reverse $ dropWhile (/= '/') (reverse arg))
  else
    checkErr tc


showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e = f ++ (showParams as) ++ " = " ++ show e ++ "\n\n"

showType :: String -> TypeVar -> Type -> String
showType acc tv t = acc ++ "type " ++ tv ++ " = " ++ show t ++ "\n\n"
  
typeChecks :: TCheckM Type -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM Type -> [String]
showErrors = snd . runWriter

codeGen :: TypeEnv -> VarEnv -> ExpEnv -> FilePath -> IO (Bool, String)
codeGen tenv venv eenv path = do 
  let types = Map.foldlWithKey showType "" tenv
  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ (showFunSignature fun (venv Map.! fun))
                           ++ showExpr fun a e) "main = putStrLn (show start)\n\n" eenv
  writeFile (path ++ "cfst.hs") (types ++ file)
  -- callCommand "ghc cfst.hs"
  -- callCommand "./cfst"
  return (True, "")

checkErr :: Map.Map TermVar (TCheckM Type)-> IO (Bool, String)
checkErr tc = do
-- putStrLn "Errors \n"   
  let err = Map.foldl (\acc v -> acc ++ showErrors v) [] tc
--  putStrLn $ intercalate "\n" err  
  return (False, intercalate "\n" err)
