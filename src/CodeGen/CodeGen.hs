module CodeGen.CodeGen
( genProgram
, HaskellCode(..)
) where 

import           Parse.Lexer (defaultPos)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Bind
import           CodeGen.DatatypeGen
import           CodeGen.ExpressionGen
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           Validation.Kinding
import           System.FilePath

-- TODO : PARAM BANG
-- TODO : start may not exist
genProgram :: VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO ()
genProgram venv eenv cenv filepath = do
  genFreeSTRunTime filepath
  let
    venv1            = updateKey venv
    eenv1            = updateKey eenv
    dataTypes        = genDataTypes cenv
    file             = translateExpEnv eenv1
    mainFun          = genMain eenv1 venv1 in
    writeFile (targetFileName filepath) (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)
--    writeFile (filepath ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)

-- TODO: export and remove from Main module
targetFileName :: String -> String
targetFileName file = replaceExtensions file "hs"

updateKey :: Map.Map PBind a -> Map.Map PBind a
updateKey m =
  let b = PBind defaultPos $ PVar "main" in
  case m Map.!? b of
   Nothing -> m
   Just e  -> Map.insert (PBind defaultPos $ PVar "_main") e (Map.delete b m)

genImports :: String
genImports = "import FreeSTRuntime\n\n"

genPragmas :: String
genPragmas = "-- Target Haskell code\n{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> VarEnv -> HaskellCode
genMain eenv venv =
  case venv Map.!? (PBind defaultPos $ PVar "_main") of
    Just t ->    
      let
--        (_,e)    = eenv Map.! (PBind defaultPos "_main")      LAMBDA
        e        = Unit defaultPos
        m        = monadicFuns eenv
        m2       = annotateAST m (PBind defaultPos $ PVar "_main") e
        h        = evalState (translate m m2 e) 0 in
      if m Map.! (PBind defaultPos $ PVar "_main") then  -- TODO: tmp start position
        "main = _main >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
      else
        "main = putStrLn (show _main)\n\n"
    Nothing -> ""


-- Generates the FreeST runtime module
genFreeSTRunTime :: FilePath -> IO ()
genFreeSTRunTime filepath =
  let path = takeDirectory filepath in
  writeFile (path ++ "/FreeSTRuntime.hs")
--    ("{-# LANGUAGE FlexibleInstances #-}\nmodule FreeSTRuntime (_fork, _new, _send, _receive, Skip) where\n\n" ++
    ("module FreeSTRuntime (_fork, _new, _send, _receive) where\n\n" ++
     genFreeSTRunTimeImports ++ "\n\n" ++
     genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
     genSend ++ "\n\n" ++ genReceive) --  ++ "\n\n" ++ genSkip)

-- genSkip :: String
-- genSkip = "type Skip = (Chan (), Chan ())\n\ninstance {-# Overlaps #-} Show Skip where\n  show _ = \"Skip\""

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
genSend = "_send ch x  = do\n  writeChan (snd ch) (unsafeCoerce x)\n  return ch"

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
