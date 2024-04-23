import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, elemIndex, intercalate, sort)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import System.Environment

main :: IO ()
main = do
    -- TODO: auto generate .md files from .fst names
    getArgs >>= mapM (\fp -> readFile fp >>= mapM print . parseFileForDocs . lines)
    return ()


-- (<doc>, <name>, <signature/definition>)
data Doc = Fun String String String
         | Data String String String
         | Type String String String
         | Section String [Doc]
         deriving (Eq)

instance Ord Doc where
    compare doc1 doc2 = compare (order doc1) (order doc2) 
        where
            order :: Doc -> Int
            order (Type _ _ _)  = 0
            order (Data _ _ _)  = 1
            order (Fun _ _ _)   = 2
            order (Section _ _) = 3

instance Show Doc where
    show (Fun name sig doc)  = showDoc  name sig doc
    show (Data name def doc) = showDoc_ name def doc
    show (Type name def doc) = showDoc_ name def doc 
    show (Section name docs) = 
        if name == "EMPTY"
        then docsStr
        else "# **" ++ name ++ "**\n" ++ docsStr
        where
            docsStr = (foldl (++) "" $ map show docs)

showDoc :: String -> String -> String -> String
showDoc name sig doc = --trace ("!!!!!!" ++ doc ++ "!!!!!!\n\n") $ 
    "## **" ++ name ++ "**\n" ++
    "**Type**: `" ++ sig ++ "`\n" ++
    -- "\n" ++
    doc ++ "\n\n"

showDoc_ :: String -> String -> String -> String
showDoc_ name def doc = 
    "## **" ++ name ++ "**\n" ++
    "**Type**: \n" ++
    "```\n" ++
    def ++ "\n" ++
    "```\n" ++
    -- "\n" ++
    doc ++ "\n\n"



parseFileForDocs :: [String] -> [Doc]
parseFileForDocs = scanForDoc [] "EMPTY" [] ""
    
scanForDoc :: [Doc] -> String -> [Doc] -> String -> [String] -> [Doc]
scanForDoc sections secName docAcc descAcc [] 
    | secName == "EMPTY" = docAcc
    | otherwise          = sections ++ [Section secName docAcc]
scanForDoc sections secName docAcc descAcc (x:xs)
    -- ignore block comments until it closes
    | isOpenBlockComment x =
        scanForDoc sections secName docAcc descAcc (drop 1 $ dropWhile (not . isCloseBlockComment) (x:xs))
    -- sections
    | isSection x =
        scanForDoc (sections ++ [Section secName (sort docAcc)]) (stripSecHeader x) [] "" xs
    -- documentation
    | isDoc x = 
        -- let (doc, xs') = captureDoc (stripDocHeader x) xs in
        -- scanForDoc sections (docAcc ++ [doc]) secName xs'
        scanForDoc sections secName docAcc (descAcc ++ "\n" ++ stripDocHeader x) xs 
    -- ignore line comments
    | isLineComment x =
        scanForDoc sections secName docAcc descAcc xs
    -- data
    | isData x  = 
        let (name, def, xs') = captureDef x xs in
        scanForDoc sections secName (docAcc ++ [Data name def descAcc]) "" xs'
    -- type
    | isType x =
        let (name, def, xs') = captureDef x xs in
        scanForDoc sections secName (docAcc ++ [Type name def descAcc]) "" xs'
    -- function
    | isFunction x = 
        let (name, sig) = captureSig x in
        -- ignore functions starting with "__", in the future, show only those exported by the module
        if "__" `isPrefixOf` name 
        then scanForDoc sections secName docAcc "" xs
        else scanForDoc sections secName (docAcc ++ [Fun name sig descAcc]) "" xs
    -- ignore all others
    | otherwise = 
        scanForDoc sections secName docAcc descAcc xs
        

captureDoc :: String -> [String] -> (Doc, [String])
captureDoc docAcc (x:xs) 
    | isDoc x   = captureDoc (docAcc ++ "\n" ++ stripDocHeader x) xs
    | isData x  = 
        let (name, def, xs') = captureDef x xs in
        (Data name def docAcc, xs') 
    | isType x  = 
        let (name, def, xs') = captureDef x xs in
        (Type name def docAcc, xs') 
    | otherwise = 
        let (name, sig) = captureSig x in
        (Fun name sig docAcc, xs)

captureSig :: String -> (String, String)
captureSig x = (strip $ take divIndex x, strip $ drop (divIndex+1) x)
    where 
        divIndex = fromJust $ elemIndex ':' x

-- (name, definition, remaining lines)
captureDef :: String -> [String] -> (String, String, [String])
captureDef defAcc [] = captureDef' defAcc []
captureDef defAcc (x:xs)
    | x == ""   = captureDef' defAcc xs
    | otherwise = captureDef (defAcc ++ "\n" ++ x) xs

captureDef' :: String -> [String] -> (String, String, [String])
captureDef' defAcc xs =
    if isJust kindIndex && fromJust kindIndex < defIndex
    then (strip $ stripDataTypeHeader $ take (fromJust kindIndex) defAcc, defAcc, xs)
    else (strip $ stripDataTypeHeader $ take defIndex defAcc, defAcc, xs)
    where
        kindIndex = elemIndex ':' defAcc
        defIndex  = fromJust $ elemIndex '=' defAcc


isDoc :: String -> Bool
isDoc = isPrefixOf "-- | "

isData :: String -> Bool
isData = isPrefixOf "data"

isType :: String -> Bool
isType = isPrefixOf "type"

-- TODO: is this right?
isFunction :: String -> Bool
isFunction s = 
    case elemIndex ':' s of
        Just i  -> all (\c -> isAlphaNum c || isSpace c || isSpecial c) $ take i s
        Nothing -> False
    where
        -- HACK
        isSpecial = not . (`elem` "\\=()")

isSection :: String -> Bool
isSection = isPrefixOf "-- # "

isOpenBlockComment, isCloseBlockComment :: String -> Bool
isOpenBlockComment  = isInfixOf "{-"
isCloseBlockComment = isInfixOf "-}"

isLineComment :: String -> Bool
isLineComment = isPrefixOf "--"


stripDocHeader :: String -> String
stripDocHeader = drop 5

stripDataTypeHeader :: String -> String
stripDataTypeHeader = drop 5

stripSecHeader :: String -> String
stripSecHeader = drop 5

strip :: String -> String
strip = reverse . lstrip . reverse . lstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs) 
    | x == ' '  = lstrip xs
    | otherwise = (x:xs)