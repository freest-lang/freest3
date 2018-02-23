{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser (
  mainProgram
) where

-- import Text.Parsec
import qualified Text.Parsec.Token as Token
import           Text.Parsec.Language (haskellDef)
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Expr
import           Types.Types
import           Terms.Terms
import           Types.Kinding
import           Types.TypeParser
import qualified Data.Map.Strict as Map


-- LEXER
lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser
        (haskellDef
        {
        Token.reservedOpNames = ["=","+","-","*","/", "mod", "rem", "&&", "||", "not"],
        Token.reservedNames = ["send","receive","()", "new", "in", "let",
                               "fork", "match", "with", "select", "case", "of",
                               "True", "False", "mod", "rev", "not", "if", "then", "else"]
        })

reservedOp = Token.reservedOp lexer
reserved   = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
decimal    = Token.decimal lexer
natural    = Token.natural lexer
lexeme     = Token.lexeme lexer
symbol     = Token.symbol lexer
parens     = Token.parens lexer
colon      = Token.colon lexer
identifier = Token.identifier lexer
comma      = Token.comma lexer

integer :: Parser Integer
integer = Token.integer lexer

apostrophe p = between (string "'") (string "'") p


-- PARSER

-- TODO : TypeEnv to VarEnv?
-- mainProgram :: FilePath -> TypeEnv -> IO (Either ParseError (TypeEnv, ExpEnv))
mainProgram :: FilePath -> VarEnv -> IO (Either ParseError (VarEnv, ExpEnv))
mainProgram filepath venv = parseFromFile (program venv) filepath

program venv =  do
    whiteSpace
    m <- manyAlternate (try parseTypeDecl) (try parseExpressionDecl) venv Map.empty
    eof
    return m

manyAlternate :: Parser (TermVar, Type) -> Parser (TermVar, (Args, Expression)) -> VarEnv -> ExpEnv -> Parser (VarEnv,ExpEnv)
manyAlternate pa pb venv eenv =
      do{as<-many1 pa; (as',bs') <- manyAlternate pa pb venv eenv; return (addListToMap as as', bs')}
  <|> do{bs<-many1 pb; (as',bs') <- manyAlternate pa pb venv eenv;  return (as', addListToMap bs bs')}
  <|> return (venv,Map.empty)
  where
    addListToMap xs m = Map.union m (Map.fromList xs)

ident = identifier <|>
      choice [try (string "(+)"), try (string "(-)"), try (string "(*)"),
              try (string "(/)"), try (string "mod"), try (string "rem"),
              try (string "(&&)"), try (string "(||)"), try (string "not")]

parseTypeDecl = do
  id <- (lexeme ident)
  colon
  colon
  t <- mainTypeParser
  if isType Map.empty t then
    return $ (id,t)
    -- return $ (id,(kindOf t, t))
    -- return $ TypeDecl id t
  else
    error $ "Type t is not well kinded: " ++ show t


parseExpressionDecl = do
  id <- identifier
  ids <- (many identifier)
  reservedOp "="
  e <- parseExpression
  return $ (id, (ids, e))--FunDecl

table = [ [binOp "*" (convertApp "(*)") AssocLeft, binOp "/" (convertApp "(/)") AssocLeft ]
        , [binOp "+" (convertApp "(+)") AssocLeft, binOp "-" (convertApp "(-)") AssocLeft,
            binary "mod" (convertApp "mod") AssocRight, binary "rem" (convertApp "rem") AssocRight ]
        , [{-TODO prefix "not" UnApplication,-} binOp "&&" (convertApp "(&&)") AssocLeft,
           binOp "||" (convertApp "(||)") AssocLeft]
        ]

convertApp op e1 e2 = (Application (Application (Variable op) e1) e2)

-- table = [[binOp "+" (convertApp "(+)") AssocLeft]]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
  -- Application
      (try $ parens parseExpression)
  -- Variables
  <|> parseVariables
  -- Conditional
  <|> parseConditional
  -- Pairs
  <|> try parsePair
  <|> parseLet
  -- Session Types
  <|> parseNew
  <|> parseSend
  <|> parseReceive
  <|> parseSelect
  -- Fork
  <|> parseFork
  -- Datatypes : TODO
  -- <|> parseValue
  -- <|> parseCase
  -- Basic expressions
  <|> try parseBasic

-- TODO Check Char type
-- TODO review read i on Integer

-- Parse Basic Types (int, bool, char and unit)

parseBasic =
      (do {c <- apostrophe anyChar; return $ Character c})
  <|> (do {b <- parseBool; return $ Boolean b})
  <|> (do {reserved "()"; return Unit})
  <|> (do {i <- many digit; return $ Integer (read i :: Int)})

parseBool =
      (do {reserved "True"; return $ True})
  <|> (do {reserved "False"; return $ False})

-- Parse Variables

parseVariables = try $ do
  id <- identifier
  return $ Variable id

-- Parse Pairs (Pair and let)

parsePair = parens $ do
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair e1 e2

parseLet = do
  reserved "let"
  id1 <- identifier
  comma
  id2 <- identifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ Let id1 id2 e1 e2

-- Parse Conditional (if then else)

parseConditional = do
  reserved "if"
  e1 <- parseExpression
  reserved "then"
  e2 <- parseExpression
  reserved "else"
  e3 <- parseExpression
  return $ Conditional e1 e2 e3

-- Parse Session Types (new, send, receive and select)

parseNew = do
  reserved "new"
  t <- mainTypeParser
  return $ New t

parseSend = do
  reserved "send"
  e1 <- parseExpression
  e2 <- parseExpression
  return $ Send e1 e2

parseReceive = do
  reserved "receive"
  e <- parseExpression
  return $ Receive e

-- TODO review Constructor for now it is an id, but it must be in camel case
parseSelect = do
  reserved "select"
  c <- identifier
  e <- parseExpression
  return $ Select c e

-- Parse Fork

parseFork = do
  reserved "fork"
  e <- parseExpression
  return $ Fork e

-- Parse Datatypes
-- parseValue
-- parseCase















--
