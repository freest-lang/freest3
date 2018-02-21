-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html
{-# LANGUAGE FlexibleContexts #-}

module Types.TypeParser (mainTypeParser) where

import           Text.Parsec.Expr
import           Text.Parsec.Language          (haskellDef)
import qualified Text.Parsec.Token             as Token
import           Text.ParserCombinators.Parsec
import qualified Data.Map.Strict               as Map
import           Types.Kinds
import           Types.Types
import           Types.Kinding


-- TODO : check list
instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m  -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> if isType Map.empty t then [(t,"")] else error $ "Type "++ (show t) ++" not well kinded"
    Left m -> error $ "type parse error " ++ show m

-- TOKENS
lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser
        (haskellDef
        {
        Token.reservedOpNames = [";", "!", "?", "->", "-o", "+", "&"],
        Token.reservedNames = ["Int","Bool","Char", "Skip", "()", "rec", "forall"]
        })

reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
comma      = Token.comma lexer
symbol     = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
lexeme     = Token.lexeme lexer
-- semi = Token.semi lexer
dot = Token.dot lexer
colon = Token.colon lexer
braces = Token.braces lexer
squares = Token.squares lexer

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

parserType :: String -> Either ParseError Type
parserType = parse mainTypeParser "Context-free Sessions (Types)"


mainTypeParser :: Parser Type
mainTypeParser =
    do{
      whiteSpace
      ; ret <- parseType
      -- ; eof
      ; return ret
  } <?> "a type: skip, T;T, ..., or ..."

parseType :: Parser Type
parseType =  lexeme $ buildExpressionParser table parseTerm

table = [ [binary "->" (Fun Un) AssocRight, binary "-o" (Fun Lin) AssocRight ]
        , [binary ";" Semi AssocLeft ]
        ]

-- binary name fun assoc = Infix  (do{ try (symbol name); return fun }) assoc
binary name fun assoc = Infix  (do{ try (symbol name); return fun }) assoc

-- prefix name fun       = Prefix (do{ reservedOp name; return fun })

parseTerm =
  try (parens parseType)
  <|> (do {  skip ;                               return Skip })
  <|> (do { b <- parseBasicType;                  return $ Basic b })
  <|> (do { try (symbol "?"); b <- parseBasicType;      return $ In b })
  <|> (do { try (symbol "!"); b <- parseBasicType;      return $ Out b })
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
  return $ PairType t u

parseRec = do
  rec
  id <- identifier
  dot
  t <- parseType
  return $ Rec id t

parseForall = do
  forall
  id <- identifier
  dot
  t <- parseType
  return $ Forall id t

parseInternalChoice = do
  reservedOp "+"
  a <- braces $ sepBy1 parseBind comma
  return $ Choice Internal (Map.fromList a)

parseExternalChoice = do
  reservedOp "&"
  a <- braces $ sepBy1 parseBind comma
  return $ Choice External (Map.fromList a)

parseDataType = do
  a <- sepBy1 parseBind comma
  return $ Datatype $ Map.fromList a

parseBind = do
  id <- identifier
  colon
  ptype <- parseType
  return (id,ptype)
