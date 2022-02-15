module Util.CmdLine where

import           Control.Monad
import           Control.Bool                  ( whenM )
import           Data.Version                   ( showVersion )
import           Paths_FreeST                   ( version
                                                )
import           Syntax.Base
-- import           System.Console.GetOpt
import           System.Directory
import           System.Exit                    ( die )
import           System.FilePath
-- import           Util.ErrorMessage (Color(..))
-- import           Util.PrettyError (formatColor, formatBold)
-- import           Util.Error
import           Util.FreestState

import Data.String
import Syntax.ProgramVariable

import Options.Applicative
-- import Options.Applicative.Help
-- import Data.Semigroup ((<>))
-- import           Data.Functor


instance Data.String.IsString ProgVar where
  fromString = mkVar defaultPos

-- instance Read ProgVar where
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
    fileDoNotExist = "\nFile " ++ f ++ " does not exist (no such file or directory)"
    wrongFileExtension = "\nFile " ++ f ++ " has not a valid file extension\n\t" ++
                         "Expecting: " ++ (f -<.> "fst") ++ "\n\t" ++
                         "but got:   " ++ f

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
-- -- -i --import ??
