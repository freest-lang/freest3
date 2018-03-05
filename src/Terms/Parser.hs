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
lexer :: Token.TokenParser ()
lexer =
  Token.makeTokenParser
    (haskellDef
       { Token.reservedOpNames =
           ["=", "+", "-", "*", "/", "mod", "rem", "&&", "||", "not", "|", "->"]
       , Token.reservedNames =
           [ "send", "receive", "()", "new", "in", "let"
           , "fork", "match", "with", "select", "case"
           , "of", "True", "False", "mod", "rev", "not"
           , "if", "then", "else", "type"
           , "data"
           ]
       }       
    )

reservedOp = Token.reservedOp lexer

reserved = Token.reserved lexer

whiteSpace = Token.whiteSpace lexer

decimal = Token.decimal lexer

natural = Token.natural lexer

lexeme = Token.lexeme lexer

symbol = Token.symbol lexer

parens = Token.parens lexer

colon = Token.colon lexer

identifier = Token.identifier lexer

comma = Token.comma lexer

apostrophe p = between (string "'") (string "'") p

integer :: Parser Integer
integer = Token.integer lexer

lowerIdentifier :: Parser [Char]
lowerIdentifier =
  lexeme $ try
      (do lc <- lower; id <- identifier; return $ [lc] ++ id)
  <|> (do lc <- lower; return [lc])

constructor :: Parser [Char]
constructor =
  lexeme $ try
       (do uc <- upper; id <- identifier; return $ [uc] ++ id)
   <|> (do uc <- (try upper); return [uc])

-- PARSER
type ParserOut = (VarEnv, ExpEnv, TypeEnv)
--type ParserOut = (VarEnv, ExpEnv, TypeEnv, ConstructorEnv)

mainProgram :: FilePath -> VarEnv -> IO (Either ParseError ParserOut)
mainProgram filepath venv = parseFromFile (program venv) filepath

program venv = do
  whiteSpace
    -- m <- manyAlternate (try parseBindingDecl) (try parseExpressionDecl) (try parseTypeAndData) venv
  m <- manyAlternate (try parseBindingDecl) (try parseExpressionDecl)
                     (try parseTypeDecl) (try parseDataType) venv
  eof
  return m

manyAlternate ::
     Parser (TermVar, Type)
  -> Parser (TermVar, (Args, Expression))
  -> Parser (TypeVar, Type)
  -> Parser (TypeVar, [(TypeVar, [Type])])
  -> VarEnv
  -> Parser ParserOut
manyAlternate pa pb pc pd venv =
     do as <- many1 pa
        (as', bs', cs') <- manyAlternate pa pb pc pd venv
        return (addListToMap as as', bs', cs')
 <|> do bs <- many1 pb
        (as', bs', cs') <- manyAlternate pa pb pc pd venv
        return (as', addListToMap bs bs', cs')
 <|> do cs <- many1 pc
        (as', bs', cs') <- manyAlternate pa pb pc pd venv
        return (as', bs', addListToMap cs cs')
 <|> do ds <- many1 pd
        (as', bs', ds') <- manyAlternate pa pb pc pd venv
        return (as', bs', addDataTypesToMap ds ds')
 <|> return (venv, Map.empty, Map.empty)
  where
   addListToMap xs m = Map.union m (Map.fromList xs) --TODO: Can't be an union (must test duplicated entries)
   addDataTypesToMap xs m = addListToMap (foldl (\acc (x, y) -> acc ++ (convertType x y)) [] xs) m

ident =
  lowerIdentifier <|>
  choice [ try (string "(+)"), try (string "(-)"), try (string "(*)")
         , try (string "(/)"), try (string "mod"), try (string "rem")
         , try (string "(&&)"), try (string "(||)"), try (string "not")
         ]

parseBindingDecl = do
  id <- (try (lexeme ident))
  colon
  colon
  t <- mainTypeParser
  return $ (id, t)
  -- if isType Map.empty t then
  --  return $ (id,t)
  -- else
    --   error $ "Type t is not well kinded: " ++ show t

parseExpressionDecl = do
  id <- try lowerIdentifier
  ids <- (many lowerIdentifier)
  reservedOp "="
  e <- parseExpression
  return (id, (ids, e)) --FunDecl

parseTypeDecl = do
  reserved "type"
  c <- constructor
  reservedOp "="
  t <- mainTypeParser
  return (c, t)-- return (c, ((kindOf t), t)) -- TODO : Kind (verify)

parseDataType = do
  reserved "data"
  c <- constructor
  reservedOp "="
  ts <- sepBy1 parseTypeComponents (lexeme (char '|'))
  -- newline
  return $ (c, ts)

parseTypeComponents = do
  c <- try constructor
  ts <- many (try mainTypeParser)
  return (c, ts)

convertType :: TypeVar -> [(TypeVar, [Type])] -> [(TypeVar, Type)]
convertType c = map (\(construct, typeList) -> (construct, conv c typeList))

conv :: TypeVar -> [Type] -> Type
conv c [] = (Var c)
conv c (x:xs) = Fun Un x (conv c xs)

-- Parses Applications
-- Builds a table that defines the priority and the associativity of each kind of Application
table =
  [ [ binOp "*" (convertApp "(*)") AssocLeft
    , binOp "/" (convertApp "(/)") AssocLeft
    ]
  , [ binOp "+" (convertApp "(+)") AssocLeft
    , binOp "-" (convertApp "(-)") AssocLeft
    , binary "mod" (convertApp "mod") AssocRight
    , binary "rem" (convertApp "rem") AssocRight
    ]
  , [ binOp "&&" (convertApp "(&&)") AssocLeft {-TODO prefix "not" UnApplication,-}
    , prefix "not" (Application (Variable "not"))
    , binOp "||" (convertApp "(||)") AssocLeft
    ]
  ]

-- Converts a binary Application in an ternary application with an operator
convertApp :: TermVar -> Expression -> Expression -> Expression
convertApp op e1 e2 = (Application (Application (Variable op) e1) e2)

binOp name fun assoc =  Infix (do reservedOp name; return fun) assoc

binary name fun assoc = Infix (do reserved name; return fun) assoc

prefix name fun =  Prefix (do reserved name; return fun)

-- Parses an expression
parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
      (try $ parens parseExpression)
  <|> parseBasic
  <|> parseConditional
  <|> try parsePair
  <|> parseLet
  <|> parseNew
  <|> parseSend
  <|> parseReceive
  <|> parseSelect
  <|> parseFork
  <|> parseCase
  <|> parseVariables
  <|> try parseValue

-- TODO Check Char type
-- TODO review read i on Integer
-- Parse Basic Types (int, bool, char and unit)
parseBasic =
      (do c <- apostrophe anyChar; return $ Character c)
  <|> parseBool
  <|> parseInteger
  <|> (do reserved "()"; return Unit)
--  <|> (do i <- many digit; return $ Integer (read i :: Int))

parseInteger = do  
  i <- integer
  return $ Integer (fromInteger i)

parseBool =
      (do reserved "True"; return $ Boolean True)
  <|> (do reserved "False"; return $ Boolean False)

-- Parse Variables
parseVariables =
  try $ do
    id <- lowerIdentifier
--    error $ "PAJE"
    return $ Variable id

-- Parse Pairs (Pair and let)
parsePair =
  parens $ do
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair e1 e2

parseLet = do
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

parseSelect = do
  reserved "select"
  c <- constructor
  e <- parseExpression
  return $ Select c e

-- Parse Fork
parseFork = do
  reserved "fork"
  e <- parseExpression
  return $ Fork e

-- Parse Datatypes
-- parseValue
parseCase = do
  reserved "case"
  e <- parseExpression
  reserved "of"
  v <- many1 parseCaseValues
  return $ Case e (Map.fromList v)

parseCaseValues = do
  c <- constructor
  ids <- (many lowerIdentifier)
  reservedOp "->"
  e <- parseExpression
  return $ (c, (ids, e))

parseValue = do
  c <- constructor
  return $ Constructor c  



