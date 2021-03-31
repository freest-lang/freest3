module FreeST
  ( main
  , checkAndRun
  )
where

import           Control.Monad.State            ( when
                                                , execState
                                                )
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        ( elaboration )
import           Interpreter.Builtin            ( initialCtx )
import           Interpreter.Eval               ( evalAndPrint )
import           Parse.Parser                   ( parseProgram )
import           Syntax.Base
import           Syntax.Program
import           System.Directory
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )
import           System.FilePath
import           Util.Error
import           Util.FreestState
import           Util.PreludeLoader             ( prelude )
import           Validation.Rename              ( renameState )
import           Validation.TypeChecking        ( typeCheck )
import           Data.Version                   ( showVersion )
import           Paths_FreeST                   ( version
                                                , getDataFileName
                                                )

main :: IO ()
main = parseOptions =<< getArgs

parseOptions :: [String] -> IO ()
parseOptions [] = throwError
  [ Error "no input file\n\t"
  , Error "Usage: For basic information, try the `--help` option."
  ]
parseOptions [x]
  | x `elem` ["-h", "--help"] = helpMenu
  | x `elem` ["-v", "--version"] = freestVersion
  | "fst" `isExtensionOf` x = do
    f <- doesFileExist x
    if f
      then checkAndRun x
      else throwError
        [ Error "File"
        , Error $ '\'' : x ++ "'"
        , Error "does not exist (No such file or directory)"
        ]
  | '-' == head x = throwError
    [ Error "Unexpected option"
    , Error $ "'" ++ x ++ "'"
    , Error "\n\t Try `freest --help` for help"
    ]        
  | otherwise = throwError
    [Error "Expecting a .fst file; found", Error x]
parseOptions (x : _) = do
  putStrLn
    $  warning "warning:"
    ++ " Found more that one option. Ignoring all but the first one."
  parseOptions [x]


throwError :: [ErrorMessage] -> IO ()
throwError = die . formatErrorMessages Map.empty defaultPos "FreeST"

helpMenu :: IO ()
helpMenu = putStrLn $ unlines
  [ styleBold "Welcome to FreeST help\n"
  , styleBold "Basic usage: " ++ "`freest path_to_file.fst`\n"
  , styleBold "Available options:\n"
  , "  --help, -h             show this help menu"
  , "  --version, -v          shows the current version"
  ]

freestVersion :: IO ()
freestVersion =
  putStrLn $ styleBold $ "The FreeST compiler, version " ++ showVersion version

warning :: String -> String
warning = styleCyan

checkAndRun :: FilePath -> IO ()
checkAndRun filePath = do
  -- Prelude
  preludeFile <- getDataFileName "Prelude.fst"
  s0          <- parseProgram preludeFile prelude
  when
    (hasErrors s0)
    (  putStrLn
    $  warning "warning: "
    ++ "Couldn't find prelude; proceeding without it"
    )
  let (initialPrelude, initialProg) = fromPreludeFile s0

  -- Parse
  s1 <- parseProgram filePath initialPrelude
  when (hasErrors s1) (die $ getErrors s1)
--  putStrLn $ "initial env: " ++  show (varEnv s1) ++ "\n\n"
--  putStrLn $ show $ parseEnv s1
  -- Solve type declarations and dualof operators
  let s2 = emptyPEnv $ execState
        elaboration
        (s1 { parseEnv = parseEnv s1 `Map.union` initialProg })
  when (hasErrors s2) (die $ getErrors s2)
--  putStrLn $ show $ prog s2
  -- Rename
  let s3 = execState renameState s2
   -- Type check
  let s4 = execState typeCheck s3
  when (hasErrors s4) (die $ getErrors s4)
  -- Interpret
  evalAndPrint initialCtx (prog s4) (prog s4 Map.! mkVar defaultPos "main")
  where
    fromPreludeFile :: FreestS -> (VarEnv, ParseEnv)
    fromPreludeFile s0
      | hasErrors s0 = (prelude, Map.empty)
      | otherwise    = (varEnv s0, parseEnv s0)
