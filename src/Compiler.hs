{-# LANGUAGE OverloadedStrings #-}
module Compiler (compile) where

import Control.Concurrent.Chan.Synchronous
import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Terms.Parser
import           Terms.Terms
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

      let a = typeCheck venv eenv cenv kenv
        
      if typeChecks a then
        codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
      else
        checkErr a
    Left err ->     
      return (False, show err)
 


showFunSignature f t = f ++ " :: " ++ show t  ++ "\n"

showExpr f as e =
  let (h, m) = evalState (translate e) 0 in
  genExpr h m
  where
    genExpr h m
      | m =
          let (x,y) = splitAt (last (findIndices (`elem` (">=" :: String)) h) + 1) h in
          f ++ (showParamsWithBang as) ++ " = \n  " ++ x ++ " return " ++ y ++ "\n\n"
      | otherwise = f ++ (showParamsWithBang as) ++ " = \n  " ++ h ++ "\n\n"

showParamsWithBang :: Params -> String
showParamsWithBang as
  | null as = ""
  | otherwise = " !" ++ (intercalate " !" as)

typeChecks :: TCheckM () -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM () -> [String]
showErrors = snd . runWriter

codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO (Bool, String)
codeGen venv eenv cenv kenv path = do
  let start = eenv Map.! "start"
  let eenv1 = Map.delete "start" eenv
  let file = Map.foldlWithKey (\acc fun (a, e) -> acc ++ showExpr fun a e) "" eenv1

  let dataMap = genDataType cenv kenv        
  writeFile (path ++ "cfst.hs") (mainFun start (venv Map.! "start")  ++ showDT dataMap ++ file) -- (types ++ file)
  return (True, "")


checkErr :: TCheckM () -> IO (Bool, String)
checkErr tc = do
  return (False, intercalate "\n" (showErrors tc))

-- Gen Datatypes
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


-- Main function, imports and communication details

mainFun :: (Params, Expression) -> Type -> String
mainFun start t =
           "{-# LANGUAGE BangPatterns #-}\n\n" ++
           genImports ++ "\n\n" ++ 
           genMain start t ++
           genCommunication ++ "\n\n"

genImports :: String
genImports = "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genMain (params, startExp) t =  
  let (h, b) = evalState (translate startExp) 0 in
  if b then
    "main = start >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n" ++
    genStart params h b 
  else
    "main = putStrLn (show start)\n\n" ++ genStart params h b ++ "\n\n"


genStart :: Params -> HaskellCode -> Bool -> String
genStart p h b
  | b = let (x,y) = splitAt (last (findIndices (`elem` (">=" :: String)) h) + 1) h in  
        "start " ++ (showParamsWithBang p) ++ " = \n  " ++ x ++ " return " ++ y ++ "\n\n"
  | otherwise =
        "start " ++ (showParamsWithBang p) ++ " = \n  " ++ h ++ "\n\n"

-- TODO Turn into an imported module
genCommunication :: String
genCommunication = genFork ++ "\n\n" ++ genNew ++ "\n\n" ++ genSend ++ "\n\n" ++ genReceive

genFork :: String
genFork = "fork e = do\n  forkIO e\n  return ()"

genNew :: String
genNew = "new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "receive ch = do\n  a <- readChan ch\n  return ((unsafeCoerce a), ch)"

-- splitAt (last (findIndices (`elem` "=") "a >>= b >>= c") + 1) "a >>= b >>= c"
