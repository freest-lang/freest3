import System.Directory

foldersName :: String
foldersName = "14 15 16a 16b 16c 16d 16e 17 18 19 20 21 22 23 24 25 26"

exN :: String
exN = "05"

main :: IO ()
main = createFiles $ words foldersName

createFiles :: [String] -> IO ()
createFiles [] = return ()
createFiles (n:ns) = do
    createDirectory (folderPath n)
    writeFile ((filePath n)++".fst") (form n)
    writeFile ((filePath n)++".expected") ""
    createFiles ns

mainFolderPath :: String -> String
mainFolderPath number = "test/Haskell Translations/ex_"++exN

folderPath :: String -> String
folderPath number = (mainFolderPath number)++"/ex_"++exN++"-"++number

filePath :: String -> String
filePath number = (folderPath number)++"/ex_"++exN++"-"++number

form :: String -> String
form number = "-- V exercise "++number++"\n\nmain : \nmain = \n--result = "