module Util.CmdLine where

import           Util.Error
import           Util.State
import           Syntax.Base

import           Control.Bool ( whenM )
import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Version ( showVersion )
import           Options.Applicative
import           Paths_FreeST ( version )
import           System.Directory
import           System.Exit ( die )
import           System.FilePath


instance Data.String.IsString Variable where
  fromString = mkVar defaultSpan

runOptsParser :: Parser RunOpts
runOptsParser = RunOpts
  <$> strArgument
     ( help "FreeST (.fst) file"
    <> metavar "FILEPATH" )
  <*> many (strArgument 
     ( help "Program arguments" 
    <> metavar "args" ))
  <*> (optional . strOption)
     ( long "main"
    <> short 'm' 
    <> help "Main function"
    <> metavar "STRING" )
  <*> flag True False    -- This is the reverse of switch
     ( long "no-colors"
    <> long "no-colours"
    <> help "Remove styles from error messages")
  <*> switch
     ( long "quiet"
    <> short 'q'
    <> help "Suppress warnings" )
  <*> flag True False
     ( long "no-sub"
    <> help "Disable subtyping")
  <*> option auto 
     ( value (6*10^4) 
    <> long "check-timeout" 
    <> help "Set timeout limit in milliseconds for equivalence/subtype checking. "
    <> metavar "TIME") 


versionParser :: String -> Parser (a -> a)
versionParser s =
  infoOption s
   (  long "version"
  <> short 'v'
  <> help "Show version" )


handleFlags :: RunOpts -> IO RunOpts
handleFlags fg@(RunOpts f _ _ sty _ _ _) = do
  whenM (not <$> doesFileExist f) $ die fileDoNotExist :: IO ()
  unless ("fst" `isExtensionOf` f) $ die wrongFileExtension
  return fg
  where
    fileDoNotExist = showError sty (Right "FreeST") Map.empty (FileNotFound f)
    wrongFileExtension = showError sty (Right "FreeST") Map.empty (WrongFileExtension f)

flags :: Bool -> IO RunOpts
flags b = handleFlags =<< execParser opts
  where
    opts = info (versionParser v <*> runOptsParser <**> helper) desc
    desc = fullDesc
     <> progDesc "Run FreeST"
     <> header (v ++ ", https://freest-lang.github.io") 
    
    v = "FreeST, Version " ++ showVersion version ++ if b then "-dev" else ""


-- criticalError :: ErrorType -> IO ()
-- criticalError = die . formatError "" Map.empty []


-- | More options ?
-- -  -p --prelude -> Custom prelude file
-- - --no-colors --no-colours -> "Remove colors from error messages"
-- --  Option Warnings as errors ??
-- -- verbose (full comment depth)
