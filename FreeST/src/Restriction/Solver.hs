{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Solver (solveInequalities) where

import           Syntax.Base
import qualified Syntax.Type as T
import qualified Restriction.Restriction as R
import           Parse.Unparser
import           Util.State
import           Restriction.Setup
import Paths_FreeST (getLibDir)

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isPrefixOf)
import Data.Char (isSpace, isDigit, isAlpha)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath ((</>), splitPath, joinPath)
import System.Process
import Control.Monad.State (liftIO)
import Text.ParserCombinators.ReadP
import Debug.Trace (trace)

data InequalityEntry = InequalityEntry
    { iSpan       :: Span
    , inequality :: R.Inequality
    } deriving (Eq, Show)

data EqualityEntry = EqualityEntry
    { eSpan       :: Span
    , equality   :: R.Equality
    } deriving (Eq, Show)

data Entry
  = EntryIneq InequalityEntry
  | EntryEq EqualityEntry
  deriving (Show)

instance ToJSON Span where
    toJSON (Span moduleName startPos endPos) =
        object [ "moduleName" .= moduleName
               , "startPos"   .= startPos
               , "endPos"     .= endPos ]

instance FromJSON Span where
    parseJSON = withObject "Span" $ \v -> do
        moduleName <- v .: "moduleName"
        startPos   <- v .: "startPos"
        endPos     <- v .: "endPos"
        return $ Span moduleName startPos endPos

instance ToJSON T.Level where
    toJSON T.Top      = String "top"
    toJSON T.Bottom   = String "bot"
    -- toJSON (T.Num n)  = Number (fromIntegral n)
    -- toJSON (T.Literal s)  = String (Text.pack s)
    toJSON (T.LVar x)  = String (Text.pack (extern x))
    toJSON (T.LNum n)  = Number (fromIntegral n)
    toJSON (T.LAdd l1 l2) = String (Text.pack (show l1 ++ "+" ++ show l2))
    toJSON (T.LParens l) = String (Text.pack ("(" ++ show l ++ ")"))

instance FromJSON T.Level where
    parseJSON (String "top")    = return T.Top
    parseJSON (String "bot")    = return T.Bottom
    parseJSON (Number n)        = return $ T.LNum (round n)
    parseJSON (String s)        = return $ parseLevel (Text.unpack s)
    parseJSON _                 = fail "Invalid T.Level format"

parseLevel :: String -> T.Level
parseLevel s =
    case [x | (x, rest) <- readP_to_S (skipSpaces *> levelP <* skipSpaces <* eof) s, all isSpace rest] of
        (l:_) -> l
        []    -> T.LVar $ mkVar defaultSpan s

levelP :: ReadP T.Level
levelP = parensP <++ addP

parensP :: ReadP T.Level
parensP = do
    skipSpaces
    _ <- char '('
    l <- levelP
    skipSpaces
    _ <- char ')'
    return (T.LParens l)

addP :: ReadP T.Level
addP = chainl1 termP addOp

addOp :: ReadP (T.Level -> T.Level -> T.Level)
addOp = do
    skipSpaces
    _ <- char '+'
    skipSpaces
    return T.LAdd

termP :: ReadP T.Level
termP = parensP <++ numP <++ varP

numP :: ReadP T.Level
numP = do
    ds <- munch1 isDigit
    return (T.LNum (read ds))

varP :: ReadP T.Level
varP = do
    v <- munch1 isAlpha
    return (T.LVar $ mkVar defaultSpan v)

instance ToJSON Entry where
    toJSON (EntryIneq ineqEntry) = toJSON ineqEntry
    toJSON (EntryEq eqEntry)     = toJSON eqEntry

instance FromJSON Entry where
  parseJSON v = withObject "Entry" (\obj -> do
    eqFlag <- obj .: "equality"
    if eqFlag == (0 :: Int)
      then EntryIneq <$> parseJSON v
      else EntryEq <$> parseJSON v
    ) v

instance ToJSON InequalityEntry where
    toJSON (InequalityEntry iSpan (l1,l2)) =
        object [ "span" .= iSpan
               , "l1" .= l1
               , "l2" .= l2 
               , "equality" .= (0 :: Int)
               ]

instance FromJSON InequalityEntry where
    parseJSON = withObject "InequalityEntry" $ \v -> do
        iSpan <- v .: "span"   
        l1      <- v .: "l1"        
        l2      <- v .: "l2"
        equality <- v .: "equality" :: Parser Int
        return $ InequalityEntry iSpan (l1, l2)

instance ToJSON EqualityEntry where
    toJSON (EqualityEntry eSpan (l1,l2)) =
        object [ "span" .= eSpan
               , "l1" .= l1
               , "l2" .= l2 
               , "equality" .= (1 :: Int)
               ]
            
instance FromJSON EqualityEntry where
    parseJSON = withObject "EqualityEntry" $ \v -> do
        eSpan <- v .: "span"   
        l1      <- v .: "l1"        
        l2      <- v .: "l2"
        equality <- v .: "equality" :: Parser Int
        return $ EqualityEntry eSpan (l1, l2)

serializeInequalities :: Inequalities -> BL.ByteString
serializeInequalities ineqs =
    encode $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList ineqs)

serializeEqualities :: Equalities -> BL.ByteString
serializeEqualities eqs =
    encode $ map (\(span, eq) -> EqualityEntry span eq) (Set.toList eqs)

deserializeEntries :: BL.ByteString -> (Inequalities, Equalities)
deserializeEntries contents =
  case decode contents :: Maybe [Entry] of
    Just entries ->
      let (ineqs, eqs) = foldr partition ([], []) entries
          partition (EntryIneq (InequalityEntry span ineq)) (is, es) = ((span, ineq):is, es)
          partition (EntryEq (EqualityEntry span eq)) (is, es) = (is, (span, eq):es)
      in (Set.fromList ineqs, Set.fromList eqs)
    Nothing -> error "Failed to parse constraints from JSON"

-- deserializeInequalities :: BL.ByteString -> Inequalities
-- deserializeInequalities contents =
--     case decode contents of
--         Just entries -> Set.fromList $ map (\(InequalityEntry span ineq) -> (span, ineq)) entries
--         Nothing      -> error "Failed to parse inequalities from JSON"

-- writeInequalitiesToFile :: Inequalities -> IO ()
-- writeInequalitiesToFile ineqs = do
--     let filteredIneqs = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") ineqs
--     let serialized = encodePretty $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList filteredIneqs)
--     filePath <- inequalitiesFilePath
--     BL.writeFile filePath serialized

-- writeEqualitiesToFile :: Equalities -> IO ()
-- writeEqualitiesToFile eqs = do
--     let filteredEqs = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") eqs
--     let serialized = encodePretty $ map (\(span, eq) -> EqualityEntry span eq) (Set.toList filteredEqs)
--     filePath <- inequalitiesFilePath
--     BL.writeFile filePath serialized

writeEntriesToFile :: Inequalities -> Equalities -> IO ()
writeEntriesToFile ineqs eqs = do
    let filteredIneqs = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") ineqs
    let filteredEqs   = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") eqs
    let entries =
          map (\(span, ineq) -> EntryIneq (InequalityEntry span ineq)) (Set.toList filteredIneqs) ++
          map (\(span, eq)   -> EntryEq   (EqualityEntry span eq))   (Set.toList filteredEqs)
    let serialized = encodePretty entries
    filePath <- inequalitiesFilePath
    BL.writeFile filePath serialized

readEntriesFromFile :: IO (Inequalities, Equalities)
readEntriesFromFile = do
    filePath <- inequalitiesFilePath
    contents <- BL.readFile filePath
    if BL.null contents
        then return (Set.empty, Set.empty)
        else do
            let (ineqs, eqs) = deserializeEntries contents
            ineqs `seq` eqs `seq` return (ineqs, eqs)


-- readInequalitiesFromFile :: IO Inequalities
-- readInequalitiesFromFile = do
--     filePath <- inequalitiesFilePath
--     contents <- BL.readFile filePath
--     if BL.null contents
--         then return Set.empty
--         else do
--             let ineqs = deserializeInequalities contents
--             ineqs `seq` return ineqs  --handle wasn't being released here, need to prevent lazy eval

getSourcePath :: IO FilePath
getSourcePath = do
    libPath <- getLibDir
    let parts = splitPath libPath
        (before, after) = break (isPrefixOf "freest3") parts
        srcPath = case after of
                    []     -> before
                    (x:_)  -> before ++ [x]
    return $ joinPath srcPath

getRestrictionModulePath :: IO FilePath
getRestrictionModulePath = do
    src <- getSourcePath
    return $ src </> "FreeST" </> "src" </> "Restriction"

inequalitiesFilePath :: IO FilePath
inequalitiesFilePath = do
    path <- getRestrictionModulePath
    return $ path </> "ineq.json"

getPythonSolverPath :: IO FilePath
getPythonSolverPath = do
    path <-getRestrictionModulePath 
    return $ path </> "solver.py"
  
solveInequalities :: Inequalities -> Equalities -> IO (Inequalities, Equalities)
solveInequalities ineqs eqs = do
    writeEntriesToFile ineqs eqs
    ineqPath <- inequalitiesFilePath
    solverPath <- getPythonSolverPath
    modulePath <- getRestrictionModulePath
    runPythonFile solverPath ineqPath modulePath
    constraints <- readEntriesFromFile
    removeFile ineqPath
    return constraints