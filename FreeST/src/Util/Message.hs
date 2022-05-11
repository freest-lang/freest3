{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Util.Message where

import           Parse.Unparser
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.GetTOps


-- | Stylable error messages

type Stylable = Bool

-- | Message class, composed by a title and an body

class Message a where
  title :: a -> Stylable -> Span -> FilePath -> String
  msg   :: a -> Stylable -> TypeOpsEnv -> String
  

class Show a => Style a where
  style :: (Stylable -> String -> String) -> Stylable -> TypeOpsEnv -> a -> String 

instance Style T.Type where
  style f sty tops  = f sty . show . getDefault tops
  
instance Style E.Exp where
  style f sty tops  = f sty . show . getDefault tops

instance Style K.Kind where
  style f sty _ = f sty . show

instance Style Variable where
  style f sty _ = f sty . quote . show

instance Style VarEnv where
  style f sty _ = f sty . show
  
instance Style FilePath where
  style f sty _ = f sty . quote 


quote :: String -> String
quote str = '\'' : str ++ "'" 

showModule :: String -> Span -> String
showModule "Prelude" _ = "Prelude"
showModule s         p' = s ++ ":" ++ show p'

-- | Styles ...

msgHeader :: String -> Stylable -> Span -> String -> String
msgHeader msgType sty p f 
  | startPos p == startPos defaultSpan && endPos p == endPos defaultSpan
                     = bold sty $ start ++ end
  | otherwise        = bold sty $ start ++ ":" ++ show p ++ end
 where
  start = "\n" ++ if null f then "FreeST" else f
  end   = ": " ++ msgType

bold :: Stylable -> String -> String
bold sty str
  | sty       = "\ESC[1m" ++ str ++ "\ESC[0m"
  | otherwise = str

red :: Stylable -> String -> String
red sty str
  | sty       = "\ESC[31m" ++ str ++ "\ESC[0m"
  | otherwise = str

-- cyan :: Stylable -> String -> String
-- cyan sty str
--   | sty       = "\ESC[36m" ++ str ++ "\ESC[0m"
--   | otherwise = str

yellow :: Stylable -> String -> String
yellow sty str
  | sty       = "\ESC[33m" ++ str ++ "\ESC[0m"
  | otherwise = str
