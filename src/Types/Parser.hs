-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html
{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Types.Parser () where

import Types.Types
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import qualified Data.Map.Strict as Map
import Types.Kinding


-- TODO : check list
instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> if isType t then [(t,"")] else error $ "Type "++ (show t) ++" not well kinded"
    Left m -> error $ "type parse error " ++ show m


-- TOKENS
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
        (haskellDef
        {
        P.reservedOpNames = [";", "!", "?", "->", "-o", "+", "&"],
        P.reservedNames = ["Int","Bool","Char", "Skip", "()", "rec", "forall"]
        })

reservedOp = P.reservedOp lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
comma      = P.comma lexer
symbol     = P.symbol lexer
whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
-- semi = P.semi lexer
dot = P.dot lexer
colon = P.colon lexer
braces = P.braces lexer
squares = P.squares lexer

rec    = reserved "rec"
forall = reserved "forall"
skip   = reserved "Skip"

-- BASIC TYPES
--   IntType | CharType | BoolType | UnitType

parserBasic :: String -> Either ParseError BasicType
parserBasic = parse parseBasicType "Context-free Sessions (Basic types)"

parseBasicType :: Parser BasicType
parseBasicType =
      (spaces >> reserved "Int"  >> spaces >> return IntType)
  <|> (spaces >> reserved "Char"  >> spaces  >> return CharType)
  <|> (spaces >> reserved "Bool"  >> spaces >> return BoolType)
  <|> (spaces >>  reserved "()" >> spaces  >> return UnitType)
  <?> "a basic type: Int, Char, Bool, or ()"

-- TYPES
-- Skip | Semi Type Type | Out BasicType | In BasicType | Basic BasicType |
-- UnFun Type Type | LinFun Type Type | Pair Type Type | ExternalChoice TypeMap |
-- InternalChoice TypeMap | Datatype TypeMap | Rec String Type | Forall String Type | Var String |

parserType :: String -> Either ParseError Type
parserType = parse mainParser "Context-free Sessions (Types)"

mainParser :: Parser Type
mainParser =
    do{
      whiteSpace
      ; ret <- parseType
      ; eof
      ; return ret
  } <?> "a type: skip, T;T, ..., or ..."

parseType :: Parser Type
parseType =  lexeme(buildExpressionParser table parseTerm)


-- parseType :: Parser Type
-- parseType =
--     do{
--       whiteSpace
--       ; ret <- lexeme(buildExpressionParser table parseTerm)
--       ; eof
--       ; return ret
--     } <?> "a type: skip, T;T, ..., or ..."

table = [ [binary "->" UnFun AssocRight, binary "-o" LinFun AssocRight ]
        , [binary ";" Semi AssocLeft ]
        ]

binary name fun assoc = Infix  (do{ Text.Parsec.try (symbol name); return fun }) assoc
-- prefix name fun       = Prefix (do{ reservedOp name; return fun })

-- TODO: remove
-- parseWithoutSpaces = do{spaces;a<-parseTerm;spaces; return a}

parseTerm =
  Text.Parsec.try (parens parseType)
  <|> (do {  skip ;                               return Skip })
  <|> (do { b <- parseBasicType;                  return $ Basic b })
  <|> (do { Text.Parsec.try (symbol "?"); b <- parseBasicType;      return $ In b })
  <|> (do { Text.Parsec.try (symbol "!"); b <- parseBasicType;      return $ Out b })
  <|> parens parsePair
  <|> parseExternalChoice
  <|> parseInternalChoice
  <|> squares parseDataType
  <|> parseRec
  <|> parseForall
  <|> (do { id <- identifier;                      return $ Var id })
  <?> "a type: Skip, T;T, !B, ?B, B, T->T, T-oT, (T,T), id, rec id.T, or forall id.t"

parsePair = do
  t <- parseType
  comma
  u <- parseType
  return $ Pair t u

parseRec = do
  rec
  id <- identifier
  dot
  t <- parseType
  return $ Rec id t

parseForall = do
  forall
  -- space
  id <- identifier
  dot
  t <- parseType
  return $ Forall id t

parseInternalChoice = do
  reservedOp "+"
  -- char '{'
  a <- braces $ sepBy1 parseBind comma
  -- char '}'
  return $ InternalChoice $ Map.fromList a

parseExternalChoice = do
  reservedOp "&"
  -- char '{'

  a <- braces $ sepBy1 parseBind comma
  -- char '}'
  return $ ExternalChoice $ Map.fromList a

parseDataType = do
  a <- sepBy1 parseBind comma
  return $ Datatype $ Map.fromList a

parseBind = do
  id <- identifier
  colon
  ptype <- parseType
  return (id,ptype)
