{-# LANGUAGE MultiWayIf #-}
module FreeST
  ( main
  , checkAndRun
  )
where

import           Control.Monad.State            ( when
                                                , execState
                                                )
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        ( elaborate )
import           Interpreter.Builtin            ( initialCtx )
import           Interpreter.Eval               ( evalAndPrint )
import           Parse.Parser                   ( parseProgram )
import           Syntax.Base
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )
import           System.FilePath                ( FilePath
                                                , isExtensionOf
                                                )
import           Utils.Error
import           Utils.FreestState
import           Utils.PreludeLoader            ( prelude )
import           Validation.Rename              ( renameState )
import           Validation.TypeChecking        ( typeCheck )

main :: IO ()
main = do
  args <- getArgs
  if
    | length args /= 1 -> die $ formatErrorMessages
      Map.empty
      defaultPos
      "FreeST"
      [Error "Incorrect number of arguments, provide just one argument"]
    | "fst" `isExtensionOf` head args -> checkAndRun (head args)
    | otherwise -> die $ formatErrorMessages
      Map.empty
      defaultPos
      "FreeST"
      [ Error "File"
      , Error $ "'" ++ head args ++ "'"
      , Error "cannot be found.\n\t"
      , Error "(Probably you haven't provided a file with extension fst)"
      ]


checkAndRun :: FilePath -> IO ()
checkAndRun filePath = do
  -- Parse
  s1 <- parseProgram filePath prelude
  when (hasErrors s1) (die $ getErrors s1)
  -- Solve type declarations and dualof operators
  let s2 = execState elaborate s1
  when (hasErrors s2) (die $ getErrors s2)
  let s2' = emptyPEnv s2
  -- Rename
  let s3  = execState renameState s2'
   -- Type check
  let s4  = execState typeCheck s3
  when (hasErrors s4) (die $ getErrors s4)
  -- Interpret
  evalAndPrint initialCtx (expEnv s4) (expEnv s4 Map.! mkVar defaultPos "main")
