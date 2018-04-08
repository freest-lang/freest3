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
           , "of", "True", "False", "mod", "rev", "not"
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
  return $ parseWith program (venv, Map.empty, Map.empty, Map.empty) file

parseWith :: CFSTParser -> ParserOut -> String -> Either ParseError ParserOut
parseWith p pout = runParser p pout "CFST Error"

program :: CFSTParser
program = do
  whiteSpace  
  many1 (try parseTypeSignature <|> try parseExpressionDecl <|> parseDataType)
  eof
  s <- getState
  return s
  -- if null err then
  --   return m
  -- else
  --   fail (intercalate "\n" err)

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
  optional $ try $ parseTypeBinding
  t <- parseType
  modifyState (changeVEnv id t)

parseTypeBinding = do
  id <- lowerIdentifier
  colon
  colon
  k <- parseKind
  string "=>"
  --modifyState (change4th id k)

parseExpressionDecl :: CFSTSubParser ()
parseExpressionDecl = do
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
  mapM (\(tc, v) -> modifyState (changeCEnv tc v)) (types c ts)
  modifyState (changeKEnv c k)
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
      lookAhead $ try $ parseExpressionDecl
  <|> (do {lookAhead $ try parseDataType})
  <|> (do {lookAhead $ try parseTypeSignature})
  <|> (do {lookAhead(try $ char '|'); return ()})
  <|> (do {lookAhead(try eof)})


-- PARSING EXPRESSIONS

-- Parses Apps
-- Builds a table that defines the priority and the associativity of each kind of App
table =
  [
    [ prefixOp "-" (App (Variable "negate"))
    ]
  ,
    [ binOp "*" (convertApp "(*)") AssocLeft
    , binOp "/" (convertApp "(/)") AssocLeft
    ]
  , [ binOp "+" (convertApp "(+)") AssocLeft
    , binOp "-" (convertApp "(-)") AssocLeft
    , binary "mod" (convertApp "mod") AssocRight
    , binary "rem" (convertApp "rem") AssocRight
    ]
  , [ binOp "&&" (convertApp "(&&)") AssocLeft
    , binOp "==" (convertApp "(==)") AssocLeft
    , prefix "not" (App (Variable "not"))
    , binOp "||" (convertApp "(||)") AssocLeft
    , binOp "<" (convertApp "(<)") AssocLeft
    , binOp ">" (convertApp "(>)") AssocLeft
    , binOp "<=" (convertApp "(<=)") AssocLeft
    , binOp ">=" (convertApp "(>=)") AssocLeft
    ]
  ]

-- Converts a binary App in an ternary application with an operator
convertApp :: TermVar -> Expression -> Expression -> Expression
convertApp op e1 e2 = (App (App (Variable op) e1) e2)

binOp name fun assoc =  Infix (do reservedOp name; return fun) assoc

binary name fun assoc = Infix (do reserved name; return fun) assoc

prefix name fun =  Prefix (do reserved name; return fun)
prefixOp name fun =  Prefix (do reservedOp name; return fun)

-- Parses an expression
parseExpression :: CFSTSubParser Expression
parseExpression =
  buildExpressionParser table (lexeme $ parseFunApp <|> constructApp <|> parseExpr)

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
  <|> parseTypeApp
  <|> parseVariable
  <|> parseConstructor
  <?> "an expression"

  -- <|> parseMatch

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
    id <- lowerIdentifier
    notFollowedBy (do {colon;colon})
    return $ Variable id

parseUnLet :: CFSTSubParser Expression
parseUnLet = try $ do
  reserved "let"
  id <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ UnLet id e1 e2

-- Parse Pairs (Pair and let)
parsePair :: CFSTSubParser Expression
parsePair =
  try $ parens $ do
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair e1 e2

parseLet :: CFSTSubParser Expression
parseLet = try $ do
  reserved "let"
  id1 <- lowerIdentifier
  comma
  id2 <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ Let id1 id2 e1 e2

-- Parse Conditional (if then else)
parseConditional :: CFSTSubParser Expression
parseConditional = do
  reserved "if"
  e1 <- parseExpression
  reserved "then"
  e2 <- parseExpression
  reserved "else"
  e3 <- parseExpression
--   error $ show e1
  return $ Conditional e1 e2 e3

-- Parse Session Types (new, send, receive and select)
parseNew :: CFSTSubParser Expression
parseNew = do
  reserved "new"
  t <- parseType
  return $ New t

parseSend :: CFSTSubParser Expression
parseSend = do
  reserved "send"
  e1 <- parseExpr
  e2 <- parseExpr
  return $ Send e1 e2

parseReceive :: CFSTSubParser Expression
parseReceive = do
  reserved "receive"
  e <- parseExpression
  return $ Receive e

parseSelect :: CFSTSubParser Expression
parseSelect = do
  reserved "select"
  c <- constructor
  e <- parseExpression
  return $ Select c e

-- Parse Fork
parseFork :: CFSTSubParser Expression
parseFork = do
  reserved "fork"
  e <- parseExpression
  return $ Fork e

-- Parse Datatypes
-- parseValue
parseCase :: CFSTSubParser Expression
parseCase = do
  reserved "case"
  e <- parseExpression
  reserved "of"
  v <- many1 parseCaseValues
  return $ Case e (Map.fromList v)

parseCaseValues :: CFSTSubParser (String, ([String], Expression))
parseCaseValues = try $ do
  c <- constructor
  ids <- (many lowerIdentifier)
  reservedOp "->"
  e <- parseExpression
  return $ (c, (ids, e))

-- parseMatch = do
--   reserved "match"
--   e <- parseExpression
--   reserved "with"
--   v <- many1 parseMatchValues
--   return $ Match e (Map.fromList v)

-- parseMatchValues = do
--   c <- constructor
--   ids <- lowerIdentifier
--   reservedOp "->"
--   e <- parseExpression
--   return $ (c, (ids, e))

parseConstructor :: CFSTSubParser Expression
parseConstructor = do
  c <- constructor
  return $ Constructor c

parseFunApp :: CFSTSubParser Expression
parseFunApp = try $ do
  c <- parseVariable
  notFollowedBy constructor
  e <- many1 parseExpr -- (try $ parens parseExpression)
  return $ foldl apply (App c (head e)) (tail e)
  where
    apply acc e = App acc e


parseTypeApp :: CFSTSubParser Expression
parseTypeApp = try $ do
  c <- parseVariable
  t <- squares $ parseType
  e <- many parseExpr -- (try $ parens parseExpression)
  return $ foldl apply (TypeApp c t) e
  where
    apply acc e = App acc e

constructApp :: CFSTSubParser Expression
constructApp = try $ do
  c <- constructor
  e <- many1 parseExpr
  return $ foldl apply (App (Constructor c) (head e)) (tail e)
  where
    apply acc e = App acc e


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

--TODO: remove (test purposes)
run = mainProgram path Map.empty
path = "src/test.hs"
--path = "/home/balmeida/tmp/testDT/dt.hs"
--path = "/home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/sendTree/oldTree"


