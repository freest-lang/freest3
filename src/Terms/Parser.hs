{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser
(
  mainProgram
) where

import qualified Data.Map.Strict as Map
import           Terms.Terms
import           Text.Parsec (Parsec (..), modifyState)
import           Text.Parsec.Expr
import           Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Token
import           Text.ParserCombinators.Parsec
import           Types.Kinding
import           Types.Kinds
import           Types.TypeParser
import           Types.Types
 
-- LEXER
-- Token.reservedNames haskellDef ++ 
lexer :: Token.TokenParser ParserOut
lexer =
  Token.makeTokenParser
    (haskellDef
       { Token.reservedOpNames =
           ["=", "+", "-", "*", "/", "mod", "rem", "&&", "||", "not", "|", "->", "==",
            ">", "<"]
       , Token.reservedNames =
           [ "send", "receive", "()", "new", "in", "let"
           , "fork", "match", "with", "select", "case"
           , "of", "True", "False"
           , "if", "then", "else", "type", "data"
           ]
       }       
    )


reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
natural = Token.natural lexer
lexeme = Token.lexeme lexer
symbol = Token.symbol lexer
parens = Token.parens lexer
colon = Token.colon lexer
identifier = Token.identifier lexer
comma = Token.comma lexer
integer = Token.integer lexer
squares = Token.squares lexer
apostrophe p = between (string "'") (string "'") p
constructor = lookAhead upper >> identifier
lowerIdentifier = lookAhead lower >> identifier


-- PARSER
type ParserOut = (VarEnv, ExpEnv, ConstructorEnv, KindEnv)
type CFSTParser = Parsec String ParserOut ParserOut
type CFSTSubParser = Parsec String ParserOut

mainProgram :: FilePath -> VarEnv -> IO (Either ParseError ParserOut)
mainProgram filepath venv = do -- parseFromFile (program venv) filepath
  file <- readFile filepath
  return $ parseWith filepath  program (venv, Map.empty, Map.empty, Map.empty) file

parseWith :: FilePath -> CFSTParser -> ParserOut -> String -> Either ParseError ParserOut
parseWith filepath p pout = runParser p pout ("CFST Error in " ++ filepath)

program :: CFSTParser
program = do
  whiteSpace  
  many1 (try parseTypeSignature <|> try parseFunction <|> parseDataType)
  eof
  s <- getState
  return s

-- MAIN PARSER COMBINATORS
-- Responsible for parsing Type Signatures, Expression Declarations and DataTypes

parseTypeSignature :: CFSTSubParser ()
parseTypeSignature = do
  pos <- getPosition
  id <- lowerIdentifier
  s <- getState
  checkDup (getVEnv s) id ("Duplicate type signatures for '" ++ id ++ "'") pos
  colon
  colon
--  optional $ try $ parseTypeBinding
  t <- parseType
  modifyState (changeVEnv id t)

parseFunction :: CFSTSubParser ()
parseFunction = do
  pos <- getPosition
  id <- try lowerIdentifier
  ids <- (many lowerIdentifier)
  reservedOp "="
  e <- parseExpression
  modifyState (changeEEnv id (ids, e)) 
  
-- parseTypeDecl :: [String] -> Parser ([String], (TermVar, Type))
-- parseTypeDecl err = do
--   reserved "type"
--   c <- constructor
--   reservedOp "="
--   t <- parseType
--   return (err, (c, t))
  -- TODO : Kind (verify)

parseDataType :: CFSTSubParser ()
parseDataType = do
  reserved "data"
  pos <- getPosition
  c <- constructor
  s <- getState
  checkDup (getKEnv s) c ("Multiple declarations of '" ++ c ++ "'") pos
  k <- option (Kind Functional Un) parseVarBind
  reservedOp "="
  ts <- sepBy1 parseTypeComponents (lexeme (char '|'))
  let bindingList = types c ts
  mapM (\(tc, v) -> modifyState (changeCEnv tc v)) bindingList
  modifyState (changeKEnv c k)
  modifyState (changeVEnv c (Datatype (Map.fromList bindingList)))
  return ()

  where
    types :: TypeVar -> [(TypeVar, [Type])] -> [(TypeVar, Type)]
    types tv = foldl (\acc (k, ts) -> acc ++ [(k, typeToFun tv ts)]) [] 

    typeToFun :: TypeVar -> [Type] -> Type
    typeToFun c [] = (Var c)
    typeToFun c (x:xs) = Fun Un x (typeToFun c xs)

parseTypeComponents :: CFSTSubParser (TypeVar, [Type])
parseTypeComponents = do
  pos <- getPosition
  c <- try constructor
  s <- getState
  checkDup (getCEnv s) c ("Multiple declarations of '" ++ c ++ "'") pos
  ts <- manyTill (try parseType) (try untilParser)
  return (c, ts)

untilParser :: CFSTSubParser ()
untilParser = do
      lookAhead $ try $ parseFunction
  <|> (do {lookAhead $ try parseDataType})
  <|> (do {lookAhead $ try parseTypeSignature})
  <|> (do {lookAhead(try $ char '|'); return ()})
  <|> (do {lookAhead(try eof)})


-- PARSING EXPRESSIONS

-- Parses Apps
-- Builds a table that defines the priority and the associativity of each kind of App
table =
  [
    [ prefixOp "-" (App (0,0) (Variable (0,0) "negate"))
    ]
  ,
    [ binOp "*" (convertApp "(*)") AssocLeft
    , binOp "/" (convertApp "(/)") AssocLeft
    ]
  , [ binOp "+" (convertApp "(+)") AssocLeft
    , binOp "-" (convertApp "(-)") AssocLeft
    , binOp "mod" (convertApp "mod") AssocRight
    , binOp "rem" (convertApp "rem") AssocRight
    ]
  , [ binOp "&&" (convertApp "(&&)") AssocLeft
    , binOp "==" (convertApp "(==)") AssocLeft
    , prefixOp "not" (App (0,0) (Variable (0,0) "not"))
    , binOp "||" (convertApp "(||)") AssocLeft
    , binOp "<" (convertApp "(<)") AssocLeft
    , binOp ">" (convertApp "(>)") AssocLeft
    , binOp "<=" (convertApp "(<=)") AssocLeft
    , binOp ">=" (convertApp "(>=)") AssocLeft
    ]
  ]

-- Converts a binary App in an ternary application with an operator
convertApp :: TermVar -> Expression -> Expression -> Expression
convertApp op e1 e2 = (App (0,0) (App (0,0) (Variable (0,0) op) e1) e2)

binOp name fun assoc =  Infix (do reservedOp name; return fun) assoc
prefixOp name fun =  Prefix (do reservedOp name; return fun)

-- binary name fun assoc = Infix (do reserved name; return fun) assoc
-- prefix name fun =  Prefix (do reserved name; return fun)

-- Parses an expression
parseExpression :: CFSTSubParser Expression
parseExpression =
  buildExpressionParser table (parseFunApp <|> parseExpr)

parseExpr :: CFSTSubParser Expression
parseExpr =
      try (parens parseExpression)
  <|> parseBasic
  <|> parseConditional
  <|> parsePair
  <|> parseUnLet
  <|> parseLet
  <|> parseNew
  <|> parseSend
  <|> parseReceive
  <|> parseSelect
  <|> parseFork
  <|> parseCase
  <|> parseMatch
  <|> parseTypeApp
  <|> constructApp
  <|> parseVariable
  <|> parseConstructor
  <?> "an expression"


-- Parse Basic Types (int, bool, char and unit)
parseBasic :: CFSTSubParser Expression
parseBasic =
      (do c <- apostrophe anyChar; return $ Character c)
  <|> parseBool
  <|> parseInteger
  <|> (do reserved "()"; return Unit)
  <?> "basic type"

parseInteger :: CFSTSubParser Expression
parseInteger = do
  i <- natural
  return $ Integer (fromInteger i)

parseBool :: CFSTSubParser Expression
parseBool =
      (do reserved "True"; return $ Boolean True)
  <|> (do reserved "False"; return $ Boolean False)
  <?> "a boolean value"
-- Parse Variables

parseVariable :: CFSTSubParser Expression
parseVariable =
  try $ do
    pos <- getPosition
    id <- lowerIdentifier
    notFollowedBy (do {colon;colon})
    return $ Variable (posPair pos) id

parseUnLet :: CFSTSubParser Expression
parseUnLet = try $ do
  reserved "let"
  pos <- getPosition
  id <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ UnLet (posPair pos) id e1 e2

-- Parse Pairs (Pair and let)
parsePair :: CFSTSubParser Expression
parsePair =
  try $ parens $ do  
    pos <- getPosition
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair (posPair pos) e1 e2

parseLet :: CFSTSubParser Expression
parseLet = try $ do
  reserved "let"
  pos <- getPosition
  id1 <- lowerIdentifier
  comma
  id2 <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ Let (posPair pos) id1 id2 e1 e2

-- Parse Conditional (if then else)
parseConditional :: CFSTSubParser Expression
parseConditional = do
  reserved "if"
  pos <- getPosition
  e1 <- parseExpression
  reserved "then"
  e2 <- parseExpression
  reserved "else"
  e3 <- parseExpression
--   error $ show e1
  return $ Conditional (posPair pos) e1 e2 e3

-- Parse Session Types (new, send, receive and select)
parseNew :: CFSTSubParser Expression
parseNew = do
  reserved "new"
  pos <- getPosition
  t <- parseType
  return $ New (posPair pos) t

parseSend :: CFSTSubParser Expression
parseSend = do
  reserved "send"
  pos <- getPosition
  e1 <- parseExpr
  e2 <- parseExpr
  return $ Send (posPair pos) e1 e2

parseReceive :: CFSTSubParser Expression
parseReceive = do
  reserved "receive"
  pos <- getPosition
  e <- parseExpression
  return $ Receive (posPair pos) e

parseSelect :: CFSTSubParser Expression
parseSelect = try $ do
  reserved "select"
  pos <- getPosition
  c <- constructor
--  notFollowedBy (constructApp <|> parseConstructor)
  e <- parseExpr
  return $ Select (posPair pos) c e

-- Parse Fork
parseFork :: CFSTSubParser Expression
parseFork = do
  reserved "fork"
  pos <- getPosition
  e <- parseExpression
  return $ Fork (posPair pos) e
  
-- Parse Datatypes
-- parseValue
parseCase :: CFSTSubParser Expression
parseCase = do
  reserved "case"
  pos <- getPosition
  e <- parseExpr
  reserved "of"
  v <- many1 parseCaseValues  
  return $ Case (posPair pos) e (Map.fromList v)

parseCaseValues :: CFSTSubParser (String, (String, Expression))
parseCaseValues = do
  c <- constructor
  id <- lowerIdentifier
  reservedOp "->"
  e <- parseExpr
  return $ (c, (id, e))

parseMatch :: CFSTSubParser Expression
parseMatch = do
  reserved "match"
  pos <- getPosition
  e <- parseExpr
  reserved "with"
  v <- many1 parseMatchValues
  return $ Match (posPair pos) e (Map.fromList v)

parseMatchValues :: CFSTSubParser (String, ([String], Expression))
parseMatchValues = do
  c <- constructor
  ids <- (many lowerIdentifier)
  reservedOp "->"
  e <- parseExpression
  return $ (c, (ids, e))

parseConstructor :: CFSTSubParser Expression
parseConstructor = do
  c <- constructor
  pos <- getPosition
  return $ Constructor (posPair pos) c

parseFunApp :: CFSTSubParser Expression
parseFunApp = try $ do
  pos <- getPosition
  c <- parseVariable
--  notFollowedBy constructor
  e <- many1 $ parseExpr -- (try $ parens parseExpression)
  let p = posPair pos
  return $ foldl (apply p) (App p c (head e)) (tail e)
  where
    apply p acc e = App p acc e


parseTypeApp :: CFSTSubParser Expression
parseTypeApp = try $ do
  pos <- getPosition
  c <- parseVariable
  ts <- many1 $ squares $ parseType
  e <- many parseExpr  -- (try $ parens parseExpression)
--  error $ show ts
  let p = posPair pos
  return $ foldl (apply p) (appTypeApp p c ts) e
  where
    appTypeApp p c ts = foldl (applyType p) (TypeApp p c (head ts)) (tail ts)
    applyType p acc t = TypeApp p acc t
    apply p acc e = App p acc e

constructApp :: CFSTSubParser Expression
constructApp = try $ do 
  pos <- getPosition
  c <- constructor
  e <- many1 parseExpr
  let p = posPair pos
  return $ foldl (apply p) (App p (Constructor p c) (head e)) (tail e)
  where
    apply p acc e = App p acc e


-- Helper functions to manage ParserOut
-- Get and insert elements for each element of ParserOut n-tuple

changeVEnv :: TermVar -> Type -> ParserOut -> ParserOut
changeVEnv k v (m1,m2,m3,m4) = (Map.insert k v m1, m2, m3, m4)

getVEnv :: ParserOut -> VarEnv
getVEnv (m1,_,_,_) = m1

changeEEnv :: TermVar -> (Params, Expression) -> ParserOut -> ParserOut
changeEEnv k v (m1,m2,m3,m4) = (m1, Map.insert k v m2, m3, m4)

changeCEnv :: TermVar -> Type -> ParserOut -> ParserOut
changeCEnv k v (m1,m2,m3,m4) = (m1, m2, Map.insert k v m3, m4)

getCEnv :: ParserOut -> ConstructorEnv
getCEnv (_,_,m3,_) = m3

changeKEnv :: TermVar -> Kind -> ParserOut -> ParserOut
changeKEnv k v (m1,m2,m3,m4) = (m1, m2, m3, Map.insert k v m4)

getKEnv :: ParserOut -> KindEnv
getKEnv (_,_,_,m4) = m4

-- Checking for duplicate declarations
checkDup :: Ord k => Map.Map k a -> k -> [Char] -> SourcePos -> CFSTSubParser ()
checkDup env id msg pos
  | Map.member id env = fail $ msg ++ position
  | otherwise         = return ()
  where position = " (line " ++ show (sourceLine pos) ++
                   ", column " ++ show (sourceColumn pos) ++ ")"


posPair :: SourcePos -> (Int, Int)
posPair pos = (sourceLine pos, sourceColumn pos)

--TODO: remove (test purposes)
run = mainProgram path Map.empty
--path = "src/test.hs"
path = "/home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/dot/dot.hs"
--path = "/home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/sendTree/sendTree.hs"

