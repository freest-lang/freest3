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

getPipPath :: FilePath -> IO FilePath
getPipPath venvPath = do
    let pipPath = if os == "mingw32"
        then venvPath </> "Scripts" </> "pip.exe"
        else venvPath </> "bin" </> "pip"
    return pipPath

getPythonPath :: FilePath -> IO FilePath
getPythonPath venvPath = do
    let pythonPath = if os == "mingw32"
        then venvPath </> "Scripts" </> "python.exe"
        else venvPath </> "bin" </> "python"
    return pythonPath

setupVenv :: FilePath -> IO FilePath
setupVenv modulePath = do
    let venvPath = modulePath </> "venv"
    pyCmd <- getPythonCommand
    venvExists <- doesDirectoryExist venvPath
    when (not venvExists) $ do
        print "Creating virtual environment for solver..."
        callProcess pyCmd ["-m", "venv", venvPath]
    return venvPath

setupZ3Lib :: FilePath -> IO ()
setupZ3Lib venvPath = do
    pipPath <- getPipPath venvPath
    (exitCode, _, _) <- readProcessWithExitCode pipPath ["show", "z3-solver"] ""
    when (exitCode /= ExitSuccess) $ callProcess pipPath ["install", "-q", "z3-solver"]

runPythonFile :: FilePath -> FilePath -> FilePath -> IO ()
runPythonFile solverPath ineqPath modulePath = do
    -- py <- isPythonInstalled
    -- when (not py) installPython --for now I'll just assume that everyone has python installed for the sake of efficiency
    venvPath <- setupVenv modulePath
    setupZ3Lib venvPath
    pyCmd <- getPythonPath venvPath 
    callProcess pyCmd [solverPath, ineqPath]