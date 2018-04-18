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

compile :: String -> IO (Bool, String)
compile arg = do
  prog <- mainProgram arg prelude

  -- (venv, eenv, cenv, kenv) <-
  --   case prog of
  --     Left err -> do
  --       putStr (show err)
  --       return $ error "Parser Error"
  --     Right d -> return d

-- print venv
  case prog of
    Right (venv, eenv, cenv, kenv) -> do
      let tc = Map.mapWithKey (\fun (a, e) -> typeCheck kenv venv cenv a e fun) eenv
    --  datatypeGen

      
      if all (== True) (Map.map typeChecks tc) then
        codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
      else
        checkErr tc
    Left err ->     
      return (False, show err)

--type T = Map.Map TypeVar [(TypeVar, Type)]
-- Map.Map TypeVar Kind
genDataType :: Map.Map TypeVar Type -> Map.Map TypeVar Kind -> Map.Map TypeVar [(TypeVar, Type)]
genDataType cenv kenv =
  Map.foldlWithKey' (\acc k _ -> Map.insert k (fromCenv cenv k) acc) Map.empty kenv

fromCenv m c =
  Map.foldlWithKey' (checkLast c) [] m

-- checkLast :: Type -> [(t, Type)] -> t -> Type -> [(t, Type)]
checkLast c acc k t
  | last tl == (Var c) = acc ++ [(k, t)]
  | otherwise = acc
  where
    tl = toList t

showDT m =
  (Map.foldlWithKey' (\acc n dl -> acc ++ "data " ++ n ++ " = " ++ intercalate " | " (showDTList dl) ++ " deriving Show\n") "" m)

showDTList l = foldl (\acc (k, v) -> acc ++ [k ++ " " ++ intercalate " " (init (showType v))]) [] l

showType (Fun Un t1 t2) = showType t1 ++ showType t2
showType t = [show t]

  

showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e = f ++ (showParamsWithBang as) ++ " = " ++ show e ++ "\n\n"

showParamsWithBang :: Params -> String
showParamsWithBang as
  | null as = ""
  | otherwise = " !" ++ (intercalate " !" as)

-- showType :: String -> TypeVar -> Type -> String
-- showType acc tv t = acc ++ "type " ++ tv ++ " = " ++ show t ++ "\n\n"
  
typeChecks :: TCheckM Type -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM Type -> [String]
showErrors = snd . runWriter

-- codeGen :: VarEnv -> ExpEnv -> FilePath -> IO (Bool, String)
codeGen venv eenv cenv kenv path = do
--  let types = Map.foldlWithKey showType "" tenv
  let file = Map.foldlWithKey (\acc fun (a, e) -> acc {-++ (showFunSignature fun (venv Map.! fun))-}
                           ++ showExpr fun a e) "" eenv
  
  let dataMap = genDataType cenv kenv
      -- putStrLn $ showDT env ++ "\n\n"             
  writeFile (path ++ "cfst.hs") (mainFun ++ showDT dataMap ++ file) -- (types ++ file)
  return (True, "")

checkErr :: Map.Map TermVar (TCheckM Type)-> IO (Bool, String)
checkErr tc = do
-- putStrLn "Errors \n"   
  let err = Map.foldl (\acc v -> acc ++ showErrors v) [] tc
--  putStrLn $ intercalate "\n" err  
  return (False, intercalate "\n" err)

mainFun :: String
mainFun = "{-# LANGUAGE BangPatterns #-}\n\n" ++
          "main = putStrLn (show start)\n\n"


