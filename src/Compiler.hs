{-# LANGUAGE OverloadedStrings #-}
module Compiler (compile) where

import           Control.Concurrent.Chan.Synchronous
import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Terms.Parser
import           Terms.Terms
import           CodeGen.CodeGen1
import           Types.Types
import           TypeChecking.TypeChecking
import           Control.Monad.Writer
import           Control.Monad.State
import           Data.List
import           System.Exit
import           Types.Kinds
import           Types.Kinding

compile :: String -> IO (Bool, String)
compile arg = do
  prog <- mainProgram arg prelude
  
  case prog of
    Right (venv, eenv, cenv, kenv) -> do
--      error $ show eenv
      let a = typeCheck venv eenv cenv kenv

      if typeChecks a then        
        codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
      else
        checkErr a
    Left err ->     
      return (False, show err)
 

-- CODE GEN

codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv ->
           FilePath -> IO (Bool, HaskellCode)
codeGen venv eenv cenv kenv path = do
  let start = eenv Map.! "start"
 -- let eenv1 = Map.delete "start" eenv
  genProgram venv eenv cenv kenv path
  -- writeFile (path ++ "cfst.hs") file
  return (True, "")

{-
codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO (Bool, HaskellCode)
codeGen venv eenv cenv kenv path = do
  let m = checkMonadicEEnv eenv
  let start = eenv Map.! "start"
  let eenv1 = Map.delete "start" eenv
  let file = genProgram eenv
        -- Map.foldlWithKey (\acc fun (a, e) -> acc ++ showExpr fun a e m) "" eenv1

  let dataMap = genDataType cenv kenv
  writeFile (path ++ "cfst.hs") (mainFun start (venv Map.! "start") m  ++ showDT dataMap ++ file) -- (types ++ file)
  return (True, "")

-- Main function, imports and communication details

mainFun :: (Params, Expression) -> TypeScheme -> MonadicMap -> String
mainFun start t m =
           "{-# LANGUAGE BangPatterns #-}\n\n" ++
           genImports ++ "\n\n" ++ 
           genMain start t m ++
           genCommunication ++ "\n\n"

genImports :: String
genImports = "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"




genStart :: Params -> HaskellCode -> Bool -> String
genStart p h b = "start " ++ (showBangParams p) ++ " = \n  " ++ h ++ "\n\n"
  -- | b =
  --   let (x,y) = splitAt (last (findIndices (`elem` (">=" :: String)) h) + 1) h in     "start " ++ (showBangParams p) ++ " = \n  " ++ x ++ " return " ++ y ++ "\n\n"
  -- | otherwise =
  --       "start " ++ (showBangParams p) ++ " = \n  " ++ h ++ "\n\n"

-- GEN EXPRESSIONS

showExpr :: TermVar -> Params -> Expression -> MonadicMap -> HaskellCode
showExpr f as e m =

  let (h, m1) = evalState (translate m e) 0 in
  genExpr h m1
  where
    genExpr h m1 = f ++ (showBangParams as) ++ " = \n  " ++ h ++ "\n\n"
      -- | m1 =
      --     let (x,y) = splitAt (last (findIndices (`elem` (">=" :: String)) h) + 1) h in
      --     f ++ (showBangParams as) ++ " = \n  " ++ x ++ " return " ++ y ++ "\n\n"
      -- | otherwise = f ++ (showBangParams as) ++ " = \n  " ++ h ++ "\n\n"

showBangParams :: Params -> String
showBangParams as
  | null as = ""
  | otherwise = " !" ++ (intercalate " !" as)

-- GEN DATATYPES

showDT :: Map.Map TypeVar [(TypeVar, TypeScheme)] -> HaskellCode
showDT m =
  Map.foldlWithKey' (\acc n dl ->
                       acc ++ "data " ++ n ++ " = " ++
                       intercalate " | " (showDTList dl) ++ " deriving Show\n") "" m
  where
    showDTList :: [(TypeVar, TypeScheme)] -> [HaskellCode]
    showDTList l =
      foldl (\acc (k, (TypeScheme _ v)) -> acc ++
              [k ++ " " ++ intercalate " " (init (showType v))]) [] l

    showType (Fun Un t1 t2) = showType t1 ++ [" "] ++ showType t2
    showType t = [show t]

genDataType :: Map.Map TypeVar TypeScheme -> Map.Map TypeVar Kind -> Map.Map TypeVar [(TypeVar, TypeScheme)]
genDataType cenv kenv =
  Map.foldlWithKey' (\acc k _ -> Map.insert k (fromCenv cenv k) acc) Map.empty kenv
  where
    fromCenv :: Map.Map TypeVar TypeScheme -> TypeVar -> [(TypeVar, TypeScheme)]
    fromCenv m c = Map.foldlWithKey' (checkLast c) [] m

    checkLast :: TypeVar -> [(TypeVar, TypeScheme)] -> TypeVar -> TypeScheme -> [(TypeVar, TypeScheme)]
    checkLast c acc k t
      | last (toList t) == (TypeScheme [] (Var c)) = acc ++ [(k, t)]
      | otherwise = acc
-}    


-- Functions to deal with typecheck monad

typeChecks :: TCheckM () -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM () -> [String]
showErrors = snd . runWriter


checkErr :: TCheckM () -> IO (Bool, String)
checkErr tc = do
  return (False, intercalate "\n" (showErrors tc))
