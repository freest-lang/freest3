import System.Directory

foldersName :: String
foldersName = "1 2a 2b 2c 2d 2e 2f 2g 2h 2i 2j 3 4 5a 5b 5c 5d 5e 5f 5g 5h 5i 6 7 8 9 10 11 12 13 14 15"

exN :: String
exN = "07"

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
form number = "-- VII exercise "++number++"\n\nmain : \nmain = \n--result = "