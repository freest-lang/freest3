module Util.CmdLine where

import           Data.Functor
import qualified Data.Map.Strict               as Map
import           Data.Version                   ( showVersion )
import           Paths_FreeST                   ( version
                                                )
import           Syntax.Base
import           System.Console.GetOpt
import           System.Directory
import           System.Exit                    ( die )
import           System.FilePath
import           Util.ErrorMessage (Color(..))
import           Util.PrettyError (formatColor, formatBold)
import           Util.Error
import           Util.FreestState


data Flag
  = Main String           -- -m --main
  | Version               -- -v --version
  | Help                  -- -h --help
  | Quiet                 -- -q --quiet
--  | Prelude String      -- -p --prelude
  deriving Show


compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (o, n)
  (_, _, errs) ->
    ioError $ userError $ concat errs ++ usageInfo helpHeader options


options :: [OptDescr Flag]
options =
  [ Option ['v'] ["version"] (NoArg Version)               "show version number"
  , Option ['h'] ["help"]    (NoArg Help)                  "show help menu"
  , Option ['m'] ["main"]    (ReqArg Main "main_function") "main function"
  , Option ['q'] ["quiet"]   (NoArg Quiet)                 "suppress warnings"
--  , Option ['p'] ["prelude"] (ReqArg Prelude "prelude_file") "prelude file"
--  , Option [] ["no-colors", "no-colours"]    (NoArg Main) "Black and white errors"
--  , Option Warnings as errors
-- verbose (full comment depth)
-- -i --import ??
  ]

helpMenu :: IO ()
helpMenu = putStrLn $ usageInfo helpHeader options

helpHeader :: String
helpHeader = "Usage: freest [OPTION...] files..."

freestVersion :: IO ()
freestVersion =
  putStrLn $ formatBold $ "FreeST, Version " ++ showVersion version


handleOpts :: ([Flag], [String]) -> IO RunOpts
handleOpts = handleOpts' initialOpts
 where
  handleOpts' :: RunOpts -> ([Flag], [String]) -> IO RunOpts
  handleOpts' opts ([], []) = throwError NoInputFile $> opts
  handleOpts' opts ([], [x]) =
    pure $ opts { runFilePath = Just x } <> defaultOpts
  handleOpts' opts (flags, xs) = do
    m <- mapM (handleFlags opts xs) flags -- join ??
    pure $ foldr (<>) initialOpts m <> defaultOpts

handleFlags :: RunOpts -> [String] -> Flag -> IO RunOpts
handleFlags opts []      (     Main _) = throwError NoInputFile $> opts
handleFlags opts [file ] (     Main s) = handleFile opts file s
handleFlags opts (x : _) flag@(Main _) = do
  putStrLn
    $  formatBold (formatColor (Just Cyan) "warning: ")
    ++ "multiple files provided. using: "
    ++ formatColor (Just Red) x
  handleFlags opts [x] flag
handleFlags opts _ Version = freestVersion $> opts
handleFlags opts _ Help    = helpMenu $> opts
handleFlags opts []   Quiet  = throwError NoInputFile $> opts
handleFlags opts [xs] Quiet  = return $ opts { runFilePath  = Just xs ,quietmode = True }
--handleFlags opts _ (Prelude prelude) =
   -- check if exists? ...
--  return $ opts { preludeFile = Just prelude }


handleFile :: RunOpts -> FilePath -> String -> IO RunOpts
handleFile opts fpath defaultMain
  | "fst" `isExtensionOf` fpath = do
    f <- doesFileExist fpath
    if f then
      pure $ opts { runFilePath  = Just fpath
                  , mainFunction = Just $ mkVar defaultPos defaultMain
                  }
    else throwError (FileNotFound fpath) $> opts
  | otherwise = die $ show fpath ++ " has not extension fst "

throwError :: ErrorType -> IO ()
throwError = die . formatError Nothing Map.empty []
