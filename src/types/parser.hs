{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Parser () where

import Types
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr

instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> [(t, "")]
    Left m -> error "basic type parse error"

-- TOKENS

-- lexer :: TokenParser ()
lexer  = P.makeTokenParser haskellDef

-- (haskellDef
         -- { P.reservedOpNames = [";", "!", "?", "->", "-o", "rec", "forall"]
         -- })

parens     = P.parens lexer
identifier = P.identifier lexer

-- BASIC TYPES

-- data BasicType =
--   IntType |
--   CharType |
--   BoolType |
--   UnitType
--   deriving (Eq, Show)

parserBasic :: String -> Either ParseError BasicType
parserBasic = parse parseBasicType "Context-free Sessions (Basic types)"

parseBasicType :: Parser BasicType
parseBasicType =
      (Text.Parsec.try (string "Int")  >> return IntType)
  <|> (Text.Parsec.try (string "Char") >> return CharType)
  <|> (Text.Parsec.try (string "Bool") >> return BoolType)
  <|> (string "()"   >> return UnitType)
  <?> "a basic type: Int, Char, Bool, or ()"

-- TYPES

-- data Type =
--   Skip |
--   Semi Type Type |
--   Out BasicType |
--   In BasicType |
--   Basic BasicType |
--   UnFun Type Type |
--   LinFun Type Type |
--   Pair Type Type |
--   Rec String Type |
--   Var String |
--   Forall String Type
--   deriving (Show)
-- TODO: internal and external choice, datatypes

-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html

parserType :: String -> Either ParseError Type
parserType = parse parseType "Context-free Sessions (Types)"

parseType :: Parser Type
parseType = buildExpressionParser table parseTerm
  <?> "a type: skip, T;T, ..., or ..."

table = [ [binary "->" UnFun AssocRight, binary "-o" LinFun AssocRight ] -- -o not working (now working)
        , [binary ";" Semi AssocLeft ]
        ]

binary name fun assoc = Infix  (do{ Text.Parsec.try(string name); return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })

parseTerm =
      parens parseType
  <|> (do { string "Skip";                 return Skip })
  <|> (do { b <- parseBasicType;           return $ Basic b })
  <|> (do { char '?'; b <- parseBasicType; return $ In b })
  <|> (do { char '!'; b <- parseBasicType; return $ Out b })
  <|> parsePair
  <|> (do { id <- identifier;              return $ Var id })
  <|> parseRec
  <|> parseForall
  <?> "a type: Skip, T;T, !B, ?B, B, T->T, T-oT, (T,T), id, rec id.T, or forall id.t"

parsePair = do
  char '('
  t <- parseType
  char ','
  u <- parseType
  char ')'
  return $ Pair t u

parseRec = do
  string "rec"
  id <- identifier
  char '.'
  t <- parseType
  return $ Rec id t

parseForall = do
  string "forall"
  id <- identifier
  char '.'
  t <- parseType
  return $ Forall id t
