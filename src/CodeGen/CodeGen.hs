module CodeGen.CodeGen
 (
   genProgram
 , HaskellCode(..)
 ) where 

import           CodeGen.DatatypeGen
import           CodeGen.ExpressionGen
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           Syntax.Terms
import           Validation.Kinding
import           Validation.TypingState(KindEnv)
import           Syntax.Types


-- TODO : PARAM BANG
-- TODO : start may not exists 
genProgram :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO ()
genProgram venv eenv cenv kenv path = do
  genUtils path
  let (_,st)          = venv Map.! "start"
      startType       = last $ toList st
      dataTypes       = genDataTypes cenv
      file            = genFile eenv
      (_,ps,mainBody) = eenv Map.! "start"
      mainFun         = genMain eenv (ps, mainBody) startType in
      writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)



genFile :: ExpEnv -> HaskellCode
genFile eenv =
  Map.foldrWithKey (\f (_,p, e) acc ->
                      acc ++ f ++ " " ++ showBangParams p ++ " = " ++
                      code f e p ++ "\n\n") "" eenv
  where 
    code f e p =
      let m = monadicFuns eenv
          --m1 = addParams p m
          m2 = fst $ isMonadic m (m Map.! f) Map.empty e in
      fst $ evalState (translate m m2 e) 0

    addParams p m = foldr (\x acc -> Map.insert x False acc) m p

showBangParams :: Params -> String
showBangParams [] = ""
--showBangParams args = intercalate " " args
showBangParams args = "!" ++ intercalate " !" args

genImports :: String
genImports = "import CFSTUtils\n\n"

genPragmas :: String
genPragmas = "{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> (Params, Expression) -> TypeScheme -> HaskellCode
genMain eenv (params, startExp) t =  
  let m = monadicFuns eenv
-- Main doesn't have parens
--      m1 = foldr (\x acc -> Map.insert x False acc) m params
--      (m2, b1) = isMonadic m1 (m1 Map.! "start") Map.empty startExp
      (m2, b1) = isMonadic m (m Map.! "start") Map.empty startExp
      (h,b) = evalState (translate m m2 startExp) 0 in
--      (h,b) = evalState (translate m1 m2 startExp) 0 in
  if b || b1 then
    "main = start >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
  else
    "main = putStrLn (show start)\n\n"

-- GENERATES THE COMMUNICATION AND THREAD CREATION MODULE

-- genUtils :: FilePath -> IO ()
-- genUtils path = do
--   b <- doesFileExist (path ++ "CFSTUtils.hs")
--   if b then return () else genUtilsFile (path ++ "CFSTUtils.hs")

-- genUtilsFile :: FilePath -> IO ()
genUtils :: FilePath -> IO ()
genUtils path =
  writeFile (path ++ "CFSTUtils.hs")
    ("module CFSTUtils (_fork, _new, _send, _receive) where\n\n" ++
     genUtilsImports ++ "\n\n" ++
     genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
     genSend ++ "\n\n" ++ genReceive)

genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)\nimport Unsafe.Coerce\n\n"


genFork :: String
genFork = "_fork e = do\n  forkIO e\n  return ()"

genNew :: String
genNew = "_new = do\n  m1 <- newEmptyMVar\n  m2 <- newEmptyMVar\n  return ((m1, m2), (m2, m1))"

genSend :: String
genSend = "_send x (m1, m2) = do\n  putMVar m2 (unsafeCoerce x)\n  return (m1, m2)"

genReceive :: String
genReceive = "_receive (m1, m2) = do\n  a <- takeMVar m1\n  return ((unsafeCoerce a), (m1, m2))"

{- With channels

genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genNew :: String
genNew = "new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "receive ch = do\n  a <- readChan ch\n  return ((unsafeCoerce a), ch)"
-}
  
