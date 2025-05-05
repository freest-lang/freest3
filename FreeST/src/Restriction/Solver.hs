{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Solver (solveInequalities) where

import           Syntax.Base
import qualified Syntax.Type as T
import qualified Restriction.Restriction as R
import           Parse.Unparser
import           Util.State

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Directory (removeFile)
import System.Process
import Control.Monad.State (liftIO)

data InequalityEntry = InequalityEntry
    { span       :: Span
    , inequality :: R.Inequality
    } deriving (Eq, Show)

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
    toJSON (T.Num n)  = Number (fromIntegral n)

instance FromJSON T.Level where
    parseJSON (String "top")    = return T.Top
    parseJSON (String "bot")    = return T.Bottom
    parseJSON (Number n)        = return $ T.Num (round n)
    parseJSON _                 = fail "Invalid T.Level format"

instance ToJSON InequalityEntry where
    toJSON (InequalityEntry span (l1,l2)) =
        object [ "span" .= span 
               , "l1" .= l1
               , "l2" .= l2 
               ]

instance FromJSON InequalityEntry where
    parseJSON = withObject "InequalityEntry" $ \v -> do
        span <- v .: "span"   
        l1      <- v .: "l1"        
        l2      <- v .: "l2"
        return $ InequalityEntry span (l1,l2)

serializeInequalities :: Inequalities -> BL.ByteString
serializeInequalities ineqs =
    encode $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList ineqs)

deserializeInequalities :: BL.ByteString -> Inequalities
deserializeInequalities contents =
    case decode contents of
        Just entries -> Set.fromList $ map (\(InequalityEntry span ineq) -> (span, ineq)) entries
        Nothing      -> error "Failed to parse inequalities from JSON"

inequalitiesFilePath :: FilePath
inequalitiesFilePath = "FreeST/src/Restriction/ineq.json"

writeInequalitiesToFile :: Inequalities -> IO ()
writeInequalitiesToFile ineqs = do
    let filteredIneqs = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") ineqs
    let serialized = encodePretty $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList filteredIneqs)
    BL.writeFile inequalitiesFilePath serialized

readInequalitiesFromFile :: IO Inequalities
readInequalitiesFromFile = do
    contents <- BL.readFile inequalitiesFilePath
    if BL.null contents
        then return Set.empty
        else do
            let ineqs = deserializeInequalities contents
            ineqs `seq` return ineqs  --handle wasn't being released here, need to prevent lazy eval
  
solveInequalities :: Inequalities -> IO Inequalities
solveInequalities ineqs = do
    writeInequalitiesToFile ineqs
    liftIO $ callProcess "python" ["FreeST/src/Restriction/solver.py"]
    ineqs <- readInequalitiesFromFile
    removeFile inequalitiesFilePath
    return ineqs