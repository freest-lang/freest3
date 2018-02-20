{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser (
--  mainProgram
) where

-- import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Types.Types
import Terms.Terms
import Types.Kinds
import Types.Kinding
import Types.TypeParser
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
{-
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
-}

mainProgram :: FilePath -> TypeEnv -> IO (Either ParseError (TypeEnv, ExpEnv))
mainProgram filepath tenv = parseFromFile (program tenv) filepath

program tenv =  do
    whiteSpace
    m <- manyAlternate (try parseTypeDecl) (try parseExpressionDecl) tenv Map.empty
    eof
    return m

manyAlternate :: Parser (TypeVar, (Kind, Type)) -> Parser (TermVar, (Args, Expression)) -> TypeEnv -> ExpEnv -> Parser (TypeEnv,ExpEnv)
manyAlternate pa pb tenv eenv =
      do{as<-many1 pa; (as',bs') <- manyAlternate pa pb tenv eenv; return (addListToMap as as', bs')}
  <|> do{bs<-many1 pb; (as',bs') <- manyAlternate pa pb tenv eenv;  return (as', addListToMap bs bs')}
  <|> return (Map.empty,Map.empty)
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
    return $ (id,(kindOf t, t))
    -- return $ TypeDecl id t
  else
    error $ "Type t is not well kinded: " ++ show t


parseExpressionDecl = do
  id <- identifier
  ids <- (many identifier)
  reservedOp "="
  e <- parseExpression
  return $ (id ,(ids, e))--FunDecl

-- table = [ [binOp "*" (Application "(*)") AssocLeft, binOp "/" (Application "(/)") AssocLeft ]
--         , [binOp "+" (Application "(+)") AssocLeft, binOp "-" (Application "(-)") AssocLeft,
--             binary "mod" (Application "mod") AssocRight, binary "rem" (Application "rem") AssocRight ]
--         , [prefix "not" (UnApplication "not"), binOp "&&" (Application "(&&)") AssocLeft,
--            binOp "||" (Application "(||)") AssocLeft]
--         ]


table = [ [binOp "*" Application AssocLeft, binOp "/" Application AssocLeft ]
        , [binOp "+" Application AssocLeft, binOp "-" Application AssocLeft,
            binary "mod" Application AssocRight, binary "rem" Application AssocRight ]
        , [{-prefix "not" UnApplication,-} binOp "&&" Application AssocLeft,
           binOp "||" Application AssocLeft]
        ]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
      (try $ parens parseExpression)
  <|> try parsePair
  <|> try parseLet
  <|> (do {id <- identifier; return $ Variable id})
  <|> try parseBasic

-- TODO Check Char type
-- TODO review read i on Integer
parseBasic =
      (do {c <- apostrophe anyChar; return $ Character c})
  <|> (do {b <- parseBool; return $ Boolean b})
  <|> (do {reserved "()"; return Unit})
  <|> (do {i <- many digit; return $ Integer (read i :: Int)})

parseBool =
      (do {reserved "True"; return $ True})
  <|> (do {reserved "False"; return $ False})

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



--
