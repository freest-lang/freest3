module Util.CmdLine where

-- import           Util.ErrorMessage (Color(..))
import           Util.Error
import           Util.FreestState
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

-- instance Read Variable where
--   readsPrec _ s = [(mkVar defaultPos s,"")]

runOptsParser :: Parser RunOpts
runOptsParser = RunOpts
  <$> strArgument
     ( help "FreeST (.fst) file"
     <> metavar "FILEPATH" )   
  <*> (optional . strOption)
      ( long "main"
     <> short 'm' 
     <> help "Main function"
     <> metavar "STRING" )
  <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "Suppress warnings" )


versionParser :: String -> Parser (a -> a)
versionParser s =
  infoOption s
   (  long "version"
  <> short 'v'
  <> help "Show version" )


handleFlags :: RunOpts -> IO RunOpts
handleFlags fg@(RunOpts f _ _) = do
  whenM (not <$> doesFileExist f) $ die fileDoNotExist :: IO ()
  when (not $ "fst" `isExtensionOf` f) $ die wrongFileExtension
  return fg
  where
    fileDoNotExist = showErrors "FreeST" Map.empty (FileNotFound f)
    wrongFileExtension = showErrors "FreeST" Map.empty (WrongFileExtension f)

flags :: IO RunOpts
flags = handleFlags =<< execParser opts
  where
    opts = info (versionParser v <*> runOptsParser <**> helper) desc
    desc = fullDesc
     <> progDesc "Run FreeST"
     <> header v
    
    v = "FreeST, Version " ++ showVersion version


-- criticalError :: ErrorType -> IO ()
-- criticalError = die . formatError "" Map.empty []


-- | More options ?
-- -  -p --prelude -> Custom prelude file
-- - --no-colors --no-colours -> "Remove colors from error messages"
-- --  Option Warnings as errors ??
-- -- verbose (full comment depth)
