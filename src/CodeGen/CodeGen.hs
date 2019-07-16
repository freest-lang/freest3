module CodeGen.CodeGen
( genProgram
-- , HaskellCode(..)
) where 

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Validation.Kinding
import           CodeGen.DatatypeGen
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           System.FilePath

import           CodeGen.Annotation
import           CodeGen.CodeGenState (HaskellCode)
import           CodeGen.Translate
import Syntax.Base
import Syntax.ProgramVariables

-- TODO : PARAM BANG
genProgram :: VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO ()
genProgram venv eenv tenv filepath = do -- return ()
  genFreeSTRunTime filepath
  let
    venv1            = updateKey venv
    eenv1            = updateKey (updateEEnv eenv) -- remove updateEenv
    dataTypes        = genDataTypes tenv
    mainFun          = genMain eenv1 venv1 tenv
    file             = translateEnv eenv1 tenv venv1  in
    writeFile (targetFileName filepath) (genPragmas ++ genImports ++ dataTypes ++ file ++ "\n\n" ++ mainFun)


-- TODO: export and remove from Main module
targetFileName :: String -> String
targetFileName file = replaceExtensions file "hs"

updateKey :: Map.Map ProgVar a -> Map.Map ProgVar a
updateKey m =
  let b = (mkVar (Pos 99999 99999) "main") in
  case m Map.!? b of
   Nothing -> m
   Just e  -> Map.insert ((mkVar (Pos 99999 99999) "_main")) e (Map.delete b m)

genImports :: String
genImports = "import FreeSTRuntime\n\n"

genPragmas :: String
genPragmas = "-- Target Haskell code\n\n"
-- genPragmas = "-- Target Haskell code\n{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> VarEnv -> TypeEnv -> HaskellCode
genMain eenv venv tenv =
  let fm = top eenv tenv venv
      mainVar = (mkVar (Pos 99999 99999) "_main")
      t    = venv Map.! mainVar
      ioT  = fm Map.! mainVar in
      if isIO ioT then
        "main = _main >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
      else
        "main = putStrLn (show _main)\n\n"

-- Generates the FreeST runtime module
genFreeSTRunTime :: FilePath -> IO ()
genFreeSTRunTime filepath =
  let path = takeDirectory filepath in
  writeFile (path ++ "/FreeSTRuntime.hs")
    ("{-# LANGUAGE FlexibleInstances #-}\n{-# LANGUAGE ScopedTypeVariables #-}\nmodule FreeSTRuntime (_fork, _new, _send, _receive, Skip, printInt, printBool, printChar, printUnit) where\n\n" ++
     genFreeSTRunTimeImports ++ "\n\n" ++ 
     genChannelEnd ++ "\n\n" ++  
     genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
     genSend ++ "\n\n" ++ genReceive ++ "\n\n" ++ genSkip ++ -- "\n\n" ++ genShowTypeable ++
     "\n\n" ++ prints)

-- genSkip :: String
-- genSkip = "type Skip = (Chan (), Chan ())\n\ninstance {-# Overlaps #-} Show Skip where\n  show _ = \"Skip\""

genFork :: String
genFork = "_fork e = do\n  forkIO e\n  return ()"

-- ONE SYNCHRONOUS CHANNEL

-- genChannelEnd :: String
-- genChannelEnd = "type ChannelEnd a b = (Chan a, Chan b)"

-- genFreeSTRunTimeImports :: String
-- genFreeSTRunTimeImports =
--   "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

-- genNew :: String
-- genNew = "_new = do\n  ch <- newChan\n  return (ch, ch)"

-- genSend :: String  
-- genSend = "_send ch x  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

-- genReceive :: String
-- genReceive = "_receive ch = do\n  a <- readChan ch\n  return (unsafeCoerce a, ch)"

-- genSkip :: String
-- genSkip = "type Skip = Chan ()\n\ninstance {-# Overlaps #-} Show Skip where\n  show _ = \"Skip\""

-- TWO ASYNCHRONOUS CHANNELS

genChannelEnd :: String
genChannelEnd = "type ChannelEnd a b = ((Chan a, Chan b), (Chan b, Chan a))"
--genChannelEnd = "type ChannelEnd a b = ((Chan (Any a), Chan (Any b)), (Chan (Any b), Chan (Any a)))"

genFreeSTRunTimeImports :: String
genFreeSTRunTimeImports =
  "import Data.Typeable\nimport Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan\nimport Unsafe.Coerce\nimport Text.Show.Functions"

genNew :: String
genNew = "_new = do\n  ch1 <- newChan\n  ch2 <- newChan\n  return ((ch1, ch2), (ch2, ch1))"

genSend :: String
genSend = "_send ch x  = do\n  writeChan (snd ch) (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "_receive ch = do\n  a <- readChan (fst ch)\n  return (unsafeCoerce a, ch)"

genSkip :: String
genSkip = "type Skip = (Chan (), Chan ())\n\ninstance {-# Overlaps #-} Show Skip where\n  show _ = \"Skip\""



-- genShowTypeable :: String
-- genShowTypeable = "instance (Typeable a, Typeable b) => Show (a->b) where\n  show _ = show $ typeOf (undefined :: a -> b)"
  
-- WITH MVARS

-- {-
-- genFreeSTRunTimeImports :: String
-- genFreeSTRunTimeImports =
--   "import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)\nimport Unsafe.Coerce\n\n"

-- genNew :: String
-- genNew = "_new = do\n  m1 <- newEmptyMVar\n  m2 <- newEmptyMVar\n  return ((m1, m2), (m2, m1))"

-- genSend :: String
-- genSend = "_send x (m1, m2) = do\n  putMVar m2 (unsafeCoerce x)\n  return (m1, m2)"

-- genReceive :: String
-- genReceive = "_receive (m1, m2) = do\n  a <- takeMVar m1\n  return ((unsafeCoerce a), (m1, m2))"
-- -}

-- Gen prints
--  "printInt :: Show a => a -> IO ()\nprintValue = putStrLn . show"
  
prints :: String
prints =
  "printInt :: Int -> IO ()\nprintInt x = putStrLn (show (x :: Int))\n" ++
  "printBool :: Bool -> IO ()\nprintBool x = putStrLn (show (x :: Bool))\n" ++
  "printChar :: Char -> IO ()\nprintChar x = putStrLn (show (x :: Char))\n" ++
  "printUnit :: () -> IO ()\nprintUnit x = putStrLn (show (x :: ()))"

-- printValue :: Show a => a -> IO ()
-- printValue = putStrLn . show
