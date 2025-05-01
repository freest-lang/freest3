{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Utils
    ( serializeInequalities
    , deserializeInequalities
    , writeInequalitiesToFile
    , readInequalitiesFromFile
    ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import Data.Text (pack, unpack)

import System.Directory (removeFile)
import System.FilePath ((</>))


import           Syntax.Base
import qualified Syntax.Type as T
import qualified Syntax.Kind as K
import qualified Restriction.Restriction as R
import           Parse.Unparser
import           Util.State

import qualified Data.Map.Strict as Map

-- Data type for JSON serialization
data InequalityEntry = InequalityEntry
    { span       :: Span
    , inequality :: R.Inequality
    } deriving (Eq, Show)

-- ToJSON and FromJSON instances for Span
instance ToJSON Span where
    toJSON (Span moduleName startPos endPos) =
        object [ "moduleName" .= moduleName
               , "startPos"   .= startPos
               , "endPos"     .= endPos ]

    -- toEncoding (Span moduleName startPos endPos) =
    --     pairs $ "moduleName" .= moduleName
    --          <> "startPos"   .= startPos
    --          <> "endPos"     .= endPos

instance FromJSON Span where
    parseJSON = withObject "Span" $ \v -> do
        moduleName <- v .: "moduleName"
        startPos   <- v .: "startPos"
        endPos     <- v .: "endPos"
        return $ Span moduleName startPos endPos

instance ToJSON T.Level where
    toJSON T.Top      = String "top"
    toJSON T.Bottom   = String "bot"
    toJSON (T.Num n)  = Number (fromIntegral n)  -- Serialize T.Num as a number

instance FromJSON T.Level where
    parseJSON (String "top")    = return T.Top
    parseJSON (String "bot")    = return T.Bottom
    parseJSON (Number n)        = return $ T.Num (round n)  -- Deserialize a number into T.Num
    parseJSON _                 = fail "Invalid T.Level format"

-- -- ToJSON and FromJSON instances for R.Inequality
-- instance ToJSON R.Inequality where
--     toJSON (l1, l2) = object [ "l1" .= l1, "l2" .= l2 ]
--     -- toEncoding (l1, l2) = pairs $ "l1" .= l1 <> "l2" .= l2

-- instance FromJSON R.Inequality where
--     parseJSON = withObject "Inequality" $ \v -> do
--         l1 <- v .: "l1"
--         l2 <- v .: "l2"
--         return (l1, l2)

-- ToJSON and FromJSON instances for InequalityEntry
instance ToJSON InequalityEntry where
    toJSON (InequalityEntry span (l1,l2)) =
        object [ "span" .= span 
               , "l1" .= l1
               , "l2" .= l2 
               ]

    -- toEncoding (InequalityEntry span inequality) =
    --     pairs $ "span" .= span <> "inequality" .= inequality

instance FromJSON InequalityEntry where
    parseJSON = withObject "InequalityEntry" $ \v -> do
        span <- v .: "span"   
        l1      <- v .: "l1"        
        l2      <- v .: "l2"
        return $ InequalityEntry span (l1,l2)

-- Function to serialize Inequalities to JSON
serializeInequalities :: Inequalities -> BL.ByteString
serializeInequalities ineqs =
    encode $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList ineqs)

-- Function to deserialize JSON into Inequalities
deserializeInequalities :: BL.ByteString -> Inequalities
deserializeInequalities contents =
    case decode contents of
        Just entries -> Set.fromList $ map (\(InequalityEntry span ineq) -> (span, ineq)) entries
        Nothing      -> error "Failed to parse inequalities from JSON"

-- Filepath for the inequalities file
inequalitiesFilePath :: FilePath
inequalitiesFilePath = "FreeST/src/Restriction/ineq.json"

-- Function to write Inequalities to a file
writeInequalitiesToFile :: Inequalities -> IO ()
writeInequalitiesToFile ineqs = do
    -- Filter out inequalities with "Prelude" as the moduleName
    let filteredIneqs = Set.filter (\(Span moduleName _ _, _) -> moduleName /= "Prelude" && moduleName /= "<default>") ineqs
    let serialized = encodePretty $ map (\(span, ineq) -> InequalityEntry span ineq) (Set.toList filteredIneqs)
    BL.writeFile inequalitiesFilePath serialized

-- Function to read Inequalities from a file, deserialize them, and delete the file
readInequalitiesFromFile :: IO Inequalities
readInequalitiesFromFile = do
    contents <- BL.readFile inequalitiesFilePath
    let ineqs = deserializeInequalities contents
    removeFile inequalitiesFilePath
    return ineqs