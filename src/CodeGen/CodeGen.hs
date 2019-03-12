module CodeGen.CodeGen
 (
   genProgram
 , HaskellCode(..)
 ) where 

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           CodeGen.DatatypeGen
import           CodeGen.ExpressionGen
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           Validation.Kinding


-- TODO : PARAM BANG
-- TODO : start may not exists 
genProgram :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO ()
genProgram venv eenv cenv kenv path = do
  genUtils path
  let
    (_,st)          = venv Map.! "start"
    (_,_,stBody)   = eenv Map.! "start"
    dataTypes       = genDataTypes cenv
    file            = translateExpEnv eenv
  -- let (_,st)          = venv Map.! "start"
--       startType       = last $ toList st
--       dataTypes       = genDataTypes cenv
--       file            = genFile eenv
--       (_,ps,mainBody) = eenv Map.! "start"
    mainFun         = genMain eenv stBody st in
    writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)



-- genFile :: ExpEnv -> HaskellCode
-- genFile eenv =
--   Map.foldrWithKey (\f (_,p, e) acc ->
--                       acc ++ f ++ " " ++ showBangParams p ++ " = " ++
--                       code f e p ++ "\n\n") "" eenv
--   where 
--     code f e p =
--       let m = monadicFuns eenv
--           --m1 = addParams p m
--           m2 = fst $ isMonadic m (m Map.! f) Map.empty e in
--       fst $ evalState (translate m m2 e) 0

--     addParams p m = foldr (\x acc -> Map.insert x False acc) m p

-- showBangParams :: Params -> String
-- showBangParams [] = ""
-- --showBangParams args = intercalate " " args
-- showBangParams args = "!" ++ intercalate " !" args

genImports :: String
genImports = "import CFSTUtils\n\n"

genPragmas :: String
genPragmas = "{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> Expression -> TypeScheme -> HaskellCode
genMain eenv startExp t =  -- "main = putStrLn \"Hello CodeGen\"\n\n"
  let m = monadicFuns eenv
--      b = (m Map.! "start")
--      m2 = annotateAST' Map.empty m b startExp
      m2 = annotateAST m "start" startExp
      h = evalState (translate m m2 startExp) 0 in
  if m Map.! "start" then
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

{-
genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)\nimport Unsafe.Coerce\n\n"


genNew :: String
genNew = "_new = do\n  m1 <- newEmptyMVar\n  m2 <- newEmptyMVar\n  return ((m1, m2), (m2, m1))"

genSend :: String
genSend = "_send x (m1, m2) = do\n  putMVar m2 (unsafeCoerce x)\n  return (m1, m2)"

genReceive :: String
genReceive = "_receive (m1, m2) = do\n  a <- takeMVar m1\n  return ((unsafeCoerce a), (m1, m2))"
-}

-- With channels

genFork :: String
genFork = "_fork e = do\n  forkIO e\n  return ()"

genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genNew :: String
genNew = "_new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "_send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "_receive ch = do\n  a <- readChan ch\n  return (unsafeCoerce a, ch)"

  
