{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser
(
  mainProgram
) where

import qualified Data.Map.Strict as Map
import           Terms.Terms
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
lexer :: Token.TokenParser ()
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

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

natural :: Parser Integer
natural = Token.natural lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

colon :: Parser String
colon = Token.colon lexer

identifier :: Parser String
identifier = Token.identifier lexer

comma :: Parser String
comma = Token.comma lexer

apostrophe :: Parser a -> Parser a
apostrophe p = between (string "'") (string "'") p

integer :: Parser Integer
integer = Token.integer lexer

constructor :: Parser String
constructor = lookAhead upper >> identifier
  
-- lowerIdentifier :: Parsec String u String
lowerIdentifier :: Parser String
lowerIdentifier = lookAhead lower >> identifier

squares :: Parser a -> Parser a
squares = Token.squares lexer

-- PARSER
--type ParserOut = (VarEnv, ExpEnv, TypeEnv)
type ParserOut = (VarEnv, ExpEnv, TypeEnv, ConstructorEnv)

mainProgram :: FilePath -> VarEnv -> IO (Either ParseError ParserOut)
mainProgram filepath venv = parseFromFile (program venv) filepath

program :: VarEnv -> Parser ParserOut
program venv = do
  whiteSpace
    -- m <- manyAlternate (try parseBindingDecl) (try parseExpressionDecl) (try parseTypeAndData) venv
  m <- manyAlternate (try parseBindingDecl) (try parseExpressionDecl)
                     (try parseTypeDecl) (try parseDataType) venv
  eof
 
  return m

manyAlternate ::
     Parser (TermVar, Type)
  -> Parser (TermVar, (Params, Expression))
  -> Parser (TypeVar, Type)
  -> Parser (TypeVar, [(TypeVar, [Type])])
  -> VarEnv
  -> Parser ParserOut
manyAlternate pa pb pc pd venv =
     do as <- many1 pa
        (as', bs', cs', ds') <- manyAlternate pa pb pc pd venv
        return (addListToMap as as', bs', cs', ds')
 <|> do bs <- many1 pb
        (as', bs', cs', ds') <- manyAlternate pa pb pc pd venv
        return (as', addListToMap bs bs', cs', ds')
 <|> do cs <- many1 pc
        (as', bs', cs', ds') <- manyAlternate pa pb pc pd venv
        return (as', bs', addListToMap cs cs', ds')
 <|> do ds <- many1 pd
        (as', bs', cs', ds') <- manyAlternate pa pb pc pd venv
        return (as', bs', cs', addDataTypesToMap ds ds')
 <|> return (venv, Map.empty, Map.empty, Map.empty)
 <?> "a funtion type declaration, a data declaration or a function declaration"
  where
   addListToMap xs m = Map.union m (Map.fromList xs) --TODO: Can't be an union (must test duplicated entries)
   addDataTypesToMap xs m = addListToMap (foldl (\acc (x, y) ->
                                          acc ++ (convertType x y)) [] xs) m


parseBindingDecl :: Parser (TermVar, Type)
parseBindingDecl = do
  id <- lowerIdentifier
  colon
  colon
  t <- parseType
  return $ (id, t)

parseExpressionDecl :: Parser (TermVar, (Params, Expression))
parseExpressionDecl = do
  id <- try lowerIdentifier
  ids <- (many lowerIdentifier)
  reservedOp "="
  e <- parseExpression
  return (id, (ids, e)) --FunDecl

parseTypeDecl :: Parser (TermVar, Type)
parseTypeDecl = do
  reserved "type"
  c <- constructor
  reservedOp "="
  t <- parseType
  return (c, t) -- TODO : Kind (verify)

parseDataType :: Parser (TypeVar, [(TypeVar, [Type])])
parseDataType = do
  reserved "data"
  c <- constructor
--  k <- optional parseVarBind
  reservedOp "="
  ts <- sepBy1 parseTypeComponents (lexeme (char '|'))
  -- newline
  return (c, ts)

parseTypeComponents :: Parser (String, [Type])
parseTypeComponents = do
  c <- try constructor
  ts <- many (try parseType)
  return (c, ts)

convertType :: TypeVar -> [(TypeVar, [Type])] -> [(TypeVar, Type)]
convertType c = map (\(construct, typeList) -> (construct, conv c typeList))

conv :: TypeVar -> [Type] -> Type
conv c [] = (Var c)
conv c (x:xs) = Fun Un x (conv c xs)

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
parseExpression :: Parser Expression
parseExpression =      
  buildExpressionParser table (lexeme $ parseFunApp <|> constructApp <|> parseExpr)

parseExpr :: Parser Expression
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
parseBasic :: Parser Expression
parseBasic =
      (do c <- apostrophe anyChar; return $ Character c)
  <|> parseBool
  <|> parseInteger
  <|> (do reserved "()"; return Unit)
  <?> "basic type"

parseInteger :: Parser Expression
parseInteger = do  
  i <- natural
  return $ Integer (fromInteger i)

parseBool :: Parser Expression
parseBool =
      (do reserved "True"; return $ Boolean True)
  <|> (do reserved "False"; return $ Boolean False)
  <?> "a boolean value"
-- Parse Variables

parseVariable :: Parser Expression
parseVariable =
  try $ do   
    id <- lowerIdentifier
    notFollowedBy (do {colon;colon})
    return $ Variable id

parseUnLet :: Parser Expression
parseUnLet = try $ do
  reserved "let"
  id <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ UnLet id e1 e2

-- Parse Pairs (Pair and let)
parsePair :: Parser Expression
parsePair =
  try $ parens $ do
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair e1 e2

parseLet :: Parser Expression
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
parseConditional :: Parser Expression
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
parseNew :: Parser Expression
parseNew = do
  reserved "new"
  t <- parseType
  return $ New t

parseSend :: Parser Expression 
parseSend = do
  reserved "send"
  e1 <- parseExpr
  e2 <- parseExpr
  return $ Send e1 e2
  
parseReceive :: Parser Expression
parseReceive = do
  reserved "receive"
  e <- parseExpression
  return $ Receive e

parseSelect :: Parser Expression
parseSelect = do
  reserved "select"
  c <- constructor
  e <- parseExpression
  return $ Select c e

-- Parse Fork
parseFork :: Parser Expression
parseFork = do
  reserved "fork"
  e <- parseExpression
  return $ Fork e

-- Parse Datatypes
-- parseValue
parseCase :: Parser Expression
parseCase = do
  reserved "case"
  e <- parseExpression
  reserved "of"
  v <- many1 parseCaseValues
  return $ Case e (Map.fromList v)

parseCaseValues :: Parser (String, ([String], Expression))
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

parseConstructor :: Parser Expression
parseConstructor = do
  c <- constructor
  return $ Constructor c
 
parseFunApp :: Parser Expression
parseFunApp = try $ do
  c <- parseVariable
  notFollowedBy constructor
  e <- many1 parseExpr -- (try $ parens parseExpression)
  return $ foldl apply (App c (head e)) (tail e)
  where
    apply acc e = App acc e


parseTypeApp :: Parser Expression
parseTypeApp = try $ do
  c <- parseVariable
  t <- squares $ parseType
  e <- many parseExpr -- (try $ parens parseExpression)  
  return $ foldl apply (TypeApp c t) e
  where
    apply acc e = App acc e

constructApp :: Parser Expression
constructApp = try $ do
  c <- constructor
  e <- many1 parseExpr
  return $ foldl apply (App (Constructor c) (head e)) (tail e)
  where
    apply acc e = App acc e
  
    
--TODO: remove (test purposes)
run = mainProgram path Map.empty
path = "src/test.hs"
--path = "/home/balmeida/tmp/testDT/dt.hs"
--path = "/home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/sendTree/oldTree"
