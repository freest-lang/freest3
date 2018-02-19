{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser (mainProgram) where

-- import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Types.Types
import Terms.Terms
import Types.Kinding
import Types.Parser
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Either

-- LEXER
lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser
        (haskellDef
        {
        Token.reservedOpNames = ["=","+","-","*","/", "mod", "rem", "&&", "||", "not"],
        Token.reservedNames = ["send","receive","()", "new", "in", "let",
                               "fork", "match", "with", "select", "case", "of",
                               "True", "False", "mod", "rev", "not"]
        -- ,Token.identStart = letter <|> oneOf ['+','-','/','*']
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

-- -- semi = Token.semi lexer
-- dot = Token.dot lexer
-- braces = Token.braces lexer
-- squares = Token.squares lexer

-- PARSER

mainProgram :: FilePath -> TypeEnv -> IO (Either ParseError (TypeEnv, ExpEnv))
mainProgram filepath env = parseFromFile (program env) filepath
  -- case () of
  --   Right m -> return m
    -- Left err -> error err

program env =  do
    whiteSpace
    (tds,vds) <- manyAlternate (try parseTypeDecl) (try parseExpressionDecl)
    eof
    return $ convertToMap env Map.empty (tds++vds)
     -- ; return (tds,vds)

manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
manyAlternate pa pb =
      do{as<-many1 pa; (as',bs') <- manyAlternate pa pb; return (as++as',bs')}
  <|> do{bs<-many1 pb; (as',bs') <- manyAlternate pa pb; return (as',bs++bs')}
  <|> return ([],[])

-- TODO: Verify if exists (Duplicated entries)
convertToMap :: TypeEnv -> ExpEnv -> [Program] -> (TypeEnv, ExpEnv)
convertToMap typeMap termMap []  = (typeMap,termMap)
convertToMap typeMap termMap (x:xs) =
  case x of
    (TypeDecl i t) -> convertToMap (Map.insert i t typeMap) termMap xs
    (FunDecl i a e) -> convertToMap typeMap (Map.insert i (a,e) termMap) xs
    _ -> error "not a type decl"

ident = identifier <|>
      choice [try (string "(+)"), try (string "(-)"), try (string "(*)"),
              try (string "(/)"), try (string "mod"), try (string "rem"),
              try (string "(&&)"), try (string "(||)"), try (string "not")]

parseTypeDecl = do
  id <- (lexeme ident)
  colon
  colon
  t <- mainTypeParser
  if isType t then
    return $ TypeDecl id t
  else
    error $ "Type t is not well kinded: " ++ show t

parseExpressionDecl = do
  id <- identifier
  ids <- (many identifier)
  reservedOp "="
  e <- parseExpression
  return $ FunDecl id ids e

table = [ [binOp "*" (App "(*)") AssocLeft, binOp "/" (App "(/)") AssocLeft ]
        , [binOp "+" (App "(+)") AssocLeft, binOp "-" (App "(-)") AssocLeft,
            binary "mod" (App "mod") AssocRight, binary "rem" (App "rem") AssocRight ]
        , [prefix "not" (UnApp "not"), binOp "&&" (App "(&&)") AssocLeft,
           binOp "||" (App "(||)") AssocLeft]
        ]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
      (try $ parens parseExpression)
  <|> parsePair
  <|> parseBasic
  <|> (do {id <- identifier; return $ Terms.Terms.Var id})


-- TODO Check Char type
parseBasic =
      (do {integer; return $ BasicTerm IntType})
  <|> parseBool
  <|> (do {apostrophe anyChar; return $ BasicTerm CharType})

parseBool =
      (do {reserved "True"; return $ BasicTerm BoolType})
  <|> (do {reserved "False"; return $ BasicTerm BoolType})


parsePair = parens $ do
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ ExpPair e1 e2



--
