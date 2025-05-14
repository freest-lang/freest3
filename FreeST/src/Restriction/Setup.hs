module Restriction.Setup (runPythonFile) where

import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist, getCurrentDirectory)
import System.Info (os)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import Control.Monad (when)

getPythonCommand :: IO String
getPythonCommand
    | os == "mingw32" = return "python"
    | otherwise       = return "python3"

isPythonInstalled :: IO Bool
isPythonInstalled = do
    pyCmd <- getPythonCommand
    (exitCode, _, _) <- readProcessWithExitCode pyCmd ["--version"] ""
    return $ exitCode == ExitSuccess

-- Install Python if not installed
installPython :: IO ()
installPython
    | os == "mingw32" = do
        callProcess "curl" ["-o", "python-installer.exe", "https://www.python.org/ftp/python/3.10.9/python-3.10.9-amd64.exe"]
        callProcess "python-installer.exe" ["/quiet", "PrependPath=1"]
        putStrLn "Python installed successfully."
    | otherwise = do
        callProcess "sudo" ["apt-get", "update"]
        callProcess "sudo" ["apt-get", "install", "-y", "python3", "python3-venv"]
        putStrLn "Python installed successfully."

isZ3LibInstalled :: IO Bool
isZ3LibInstalled = do
    (exitCode, _, _) <- readProcessWithExitCode "pip" ["show", "z3-solver"] ""
    return $ exitCode == ExitSuccess

installZ3Lib :: IO ()
installZ3Lib = do
    z3 <- isZ3LibInstalled
    when (not z3) $ do
        callProcess "pip" ["install", "-q", "z3-solver"]

runPythonFile :: FilePath -> FilePath -> IO ()
runPythonFile solverPath ineqPath = do
    -- py <- isPythonInstalled
    -- when (not py) installPython
    pyCmd <- getPythonCommand
    installZ3Lib
    callProcess pyCmd [solverPath, ineqPath]