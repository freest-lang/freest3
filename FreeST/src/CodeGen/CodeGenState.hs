module CodeGen.CodeGenState where

import           Control.Monad.State
import           Syntax.Expressions
import           Syntax.ProgramVariables
import qualified Data.Map.Strict as Map
import           Syntax.Show

type AST = Map.Map Expression NodeType
type AnnFunMap = Map.Map ProgVar NodeType

data NodeType =
    IOType
  | PureType
  | ArrowType NodeType NodeType
  deriving Eq

-- Show fun create
instance Show NodeType where
  show IOType = "io"
  show PureType = "p"
  show (ArrowType t1 t2) = show t1 ++ "_" ++ show t2
 
instance Ord NodeType where
  a1@(ArrowType _ _)   <= a2@(ArrowType _ _) = lengthNT a1 <= lengthNT a2
  IOType   <= (ArrowType _ _) = True  
  PureType   <= (ArrowType _ _) = True  
  PureType <= IOType          = True  
  _        <= _               = False

lengthNT :: NodeType -> Int
lengthNT (ArrowType t1 t2) = lengthNT t1 + lengthNT t2
lengthNT t                 = 1

type HaskellCode = String
type GenFunsMap = Map.Map String HaskellCode

data TranslateState = TranslateState {
  nextIndex  :: Int
, genFunsMap :: GenFunsMap
, haskellCode :: HaskellCode
, annFunMap :: AnnFunMap
, ast :: AST
}

type TranslateM = State TranslateState

initialState :: AnnFunMap -> AST -> TranslateState
initialState m ast = TranslateState {
  nextIndex  = 0
, genFunsMap = Map.empty
, haskellCode = ""
, annFunMap = m
, ast = ast
}
  
-- Gets the next fresh var based on the state
nextFresh :: TranslateM String
nextFresh = do
  s <- get
  let next = nextIndex s
  modify (\s -> s{nextIndex = next + 1})
  return $ "_x" ++ show next

-- Adds an element to the map
addFun :: String -> HaskellCode -> TranslateM ()
addFun k v =
  modify (\s -> s{genFunsMap = Map.insert k v (genFunsMap s)})

funMember :: String -> TranslateM Bool
funMember f = do
  m <- getGenFunsMap
  return $ Map.member f m

getGenFunsMap :: TranslateM GenFunsMap
getGenFunsMap = do 
  s <- get
  return $ genFunsMap s

getAnnFunMap :: TranslateM AnnFunMap
getAnnFunMap = do 
  s <- get
  return $ annFunMap s

getAST :: TranslateM AST
getAST = do 
  s <- get
  return $ ast s

getFromAST :: Expression -> TranslateM NodeType
getFromAST e = do 
  m <- getAST
  return $ m Map.! e

insertAST :: Expression -> NodeType -> TranslateM ()
insertAST k v = 
  modify (\s -> s{ast = Map.insert k v (ast s)})

getHaskellCode :: TranslateM HaskellCode
getHaskellCode = do
  s <- get
  return $ haskellCode s
  
addHaskellCode :: ProgVar -> HaskellCode -> TranslateM ()
addHaskellCode fname c =  
  modify (\s -> s{haskellCode = (haskellCode s) ++ "\n\n" ++
                   show fname ++ " = " ++ c})
