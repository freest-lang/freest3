module CodeGen.CodeGen
( genProgram
, HaskellCode(..)
) where 

import           Parse.Lexer (defaultPos)
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
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
  genFreeSTRunTime path
  let
    venv1            = updateKey venv
    eenv1            = updateKey eenv
    dataTypes       = genDataTypes cenv
    file            = translateExpEnv eenv1
    mainFun         = genMain eenv1 venv1 in
    writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)

updateKey :: Map.Map Bind a -> Map.Map Bind a
updateKey m =
  let b = Bind defaultPos "main" in
  case m Map.!? b of
   Nothing -> m
   Just e  -> Map.insert (Bind defaultPos "_main") e (Map.delete b m)

genImports :: String
genImports = "import FreeSTRuntime\n\n"

genPragmas :: String
genPragmas = "-- Target Haskell code\n{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> VarEnv -> HaskellCode
genMain eenv venv =
  case venv Map.!? (Bind defaultPos "_main") of
    Just t ->    
      let
        (_,e)    = eenv Map.! (Bind defaultPos "_main")
        m        = monadicFuns eenv
        m2       = annotateAST m (Bind defaultPos "_main") e
        h        = evalState (translate m m2 e) 0 in
      if m Map.! (Bind defaultPos "_main") then  -- TODO: tmp start position
        "main = _main >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
      else
        "main = putStrLn (show _main)\n\n"
    Nothing -> ""


-- Generates the FreeST runtime module
genFreeSTRunTime :: FilePath -> IO ()
genFreeSTRunTime path =
  writeFile (path ++ "FreeSTRuntime.hs")
    ("module FreeSTRuntime (_fork, _new, _send, _receive) where\n\n" ++
     genFreeSTRunTimeImports ++ "\n\n" ++
     genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
     genSend ++ "\n\n" ++ genReceive)

genFork :: String
genFork = "_fork e = do\n  forkIO e\n  return ()"

-- With channels
-- Two channels
genFreeSTRunTimeImports :: String
genFreeSTRunTimeImports =
  "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genNew :: String
genNew = "_new = do\n  ch1 <- newChan\n  ch2 <- newChan\n  return ((ch1,ch2), (ch2,ch1))"

genSend :: String
genSend = "_send x ch  = do\n  writeChan (snd ch) (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "_receive ch = do\n  a <- readChan (fst ch)\n  return (unsafeCoerce a, ch)"

{-
-- one channel
genFreeSTRunTimeImports :: String
genFreeSTRunTimeImports =
  "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genNew :: String
genNew = "_new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "_send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "_receive ch = do\n  a <- readChan ch\n  return (unsafeCoerce a, ch)"
-}
  
-- With MVar

{-
genFreeSTRunTimeImports :: String
genFreeSTRunTimeImports =
  "import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)\nimport Unsafe.Coerce\n\n"

genNew :: String
genNew = "_new = do\n  m1 <- newEmptyMVar\n  m2 <- newEmptyMVar\n  return ((m1, m2), (m2, m1))"

genSend :: String
genSend = "_send x (m1, m2) = do\n  putMVar m2 (unsafeCoerce x)\n  return (m1, m2)"

genReceive :: String
genReceive = "_receive (m1, m2) = do\n  a <- takeMVar m1\n  return ((unsafeCoerce a), (m1, m2))"
-}
