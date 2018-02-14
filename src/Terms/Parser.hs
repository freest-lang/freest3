{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser (mainProgram) where

import Text.Parsec
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

integer :: Parser Integer
integer = Token.integer lexer


apostrophe p = between (string "'") (string "'") p

-- comma      = Token.comma lexer
-- -- semi = Token.semi lexer
-- dot = Token.dot lexer
-- braces = Token.braces lexer
-- squares = Token.squares lexer
-- send = reserved "send"

-- PARSER

mainProgram :: FilePath -> IO (Either ParseError (TypeEnv, ExpEnv))
mainProgram filepath = parseFromFile program filepath
  -- case () of
  --   Right m -> return m
    -- Left err -> error err

program =  do{whiteSpace
             ;(tds,vds) <- manyAlternate (Text.Parsec.try parseTypeDecl) (Text.Parsec.try parseExpressionDecl)
             ;eof
             ;return $ convertToMap Map.empty Map.empty (tds++vds)
             -- ; return (tds,vds)
             }

manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
manyAlternate pa pb = do{as<-many1 pa; (as',bs') <- manyAlternate pa pb; return (as++as',bs')}
                      <|>
                      do{bs<-many1 pb; (as',bs') <- manyAlternate pa pb; return (as',bs++bs')}
                      <|>
                      return ([],[])

convertToMap :: TypeEnv -> ExpEnv -> [Program] -> (TypeEnv, ExpEnv)
convertToMap typeMap termMap []  = (typeMap,termMap)
convertToMap typeMap termMap (x:xs) =
  case x of
    (TypeDecl i t) -> convertToMap (Map.insert i t typeMap) termMap xs
    (FunDecl i a e) -> convertToMap typeMap (Map.insert i (a,e) termMap) xs
    _ -> error "not a type decl"
  -- | FunDecl i Args Expression


parseTypeDecl = do
  id <- identifier
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

--TODO: mod and rev Associativity and precedence
--TODO: bool priority

table = [ [binOp "*" (IntApp "*") AssocLeft, binOp "/" (IntApp "/") AssocLeft ]
        , [binOp "+" (IntApp "+") AssocLeft, binOp "-" (IntApp "-") AssocLeft,
            binary "mod" (IntApp "mod") AssocLeft, binary "rem" (IntApp "rem") AssocLeft ]
        , [binOp "&&" (BoolApp "&&") AssocLeft, binOp "||" (BoolApp "||") AssocLeft
          , prefix "not" (UnBoolApp "not")
          ]
        ]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

-- table = []
-- parseExpression = buildExpressionParser table (lexeme parseExpr)
parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
      (parens parseExpression)
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


--
