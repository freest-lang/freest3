-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html
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
    Left m -> error "type parse error"

-- TOKENS

-- lexer :: TokenParser ()
lexer  = P.makeTokenParser haskellDef

-- (haskellDef
         -- { P.reservedOpNames = [";", "!", "?", "->", "-o", "rec", "forall"]
         -- })

parens     = P.parens lexer
identifier = P.identifier lexer

-- BASIC TYPES
--   IntType | CharType | BoolType | UnitType

parserBasic :: String -> Either ParseError BasicType
parserBasic = parse parseBasicType "Context-free Sessions (Basic types)"

parseBasicType :: Parser BasicType
parseBasicType =
      (spaces >> Text.Parsec.try (string "Int")  >> spaces >> return IntType)
  <|> (spaces >> Text.Parsec.try (string "Char")  >> spaces  >> return CharType)
  <|> (spaces >> Text.Parsec.try (string "Bool")  >> spaces >> return BoolType)
  <|> (spaces >>  Text.Parsec.try (string "()") >> spaces  >> return UnitType)
  <?> "a basic type: Int, Char, Bool, or ()"

-- TYPES
-- Skip | Semi Type Type | Out BasicType | In BasicType | Basic BasicType |
-- UnFun Type Type | LinFun Type Type | Pair Type Type | ExternalChoice TypeMap |
-- InternalChoice TypeMap | Datatype TypeMap | Rec String Type | Forall String Type | Var String |

parserType :: String -> Either ParseError Type
parserType = parse parseType "Context-free Sessions (Types)"

parseType :: Parser Type
parseType = buildExpressionParser table parseWithoutSpaces
  <?> "a type: skip, T;T, ..., or ..."

table = [ [binary "->" UnFun AssocRight, binary "-o" LinFun AssocRight ] -- -o not working (now working)
        , [binary ";" Semi AssocLeft ]
        ]

binary name fun assoc = Infix  (do{ Text.Parsec.try(string name); return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })

parseWithoutSpaces = do{spaces;a<-parseTerm;spaces; return a}

parseTerm =
      -- parens parseType
   (do { string "Skip";               return Skip })
  <|> (do { b <- parseBasicType;           return $ Basic b })
  <|> (do { char '?'; b <- parseBasicType; return $ In b })
  <|> (do { char '!'; b <- parseBasicType; return $ Out b })
  <|> parsePair
  <|> parseRec
  <|> parseForall
  <|> (do { id <- identifier;              return $ Var id })
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
  space
  id <- identifier
  char '.'
  t <- parseType
  return $ Rec id t

parseForall = do
  string "forall"
  space
  id <- identifier
  char '.'
  t <- parseType
  return $ Forall id t
