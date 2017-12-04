-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html
{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Parser () where

import Types
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import qualified Data.Map.Strict as Map

-- TODO : check list
instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> [(t, "")]
    Left m -> error "type parse error"


-- TOKENS

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
        (haskellDef
        {P.reservedNames = ["Int","Bool","Char", "Skip", "rec", "forall"{-unit? ,()-}]
         ,P.reservedOpNames = [";", "!", "?", "->", "-o", "+", "&"]
        })

reservedOp = P.reservedOp lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
comma      = P.comma lexer

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

table = [ [binary "->" UnFun AssocRight, binary "-o" LinFun AssocRight ]
        , [binary ";" Semi AssocLeft ]
        ]

binary name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })

parseWithoutSpaces = do{spaces;a<-parseTerm;spaces; return a}

parseTerm =
  Text.Parsec.try (parens parseType)
  <|>  (do { skip;                     return Skip })
  <|> (do { b <- parseBasicType;            return $ Basic b })
  <|> (do { reservedOp "?"; b <- parseBasicType;  return $ In b })
  <|> (do { reservedOp "!"; b <- parseBasicType;  return $ Out b })
  <|> parsePair
  <|> parseExternalChoice
  <|> parseInternalChoice
  <|> parseDataType
  <|> parseRec
  <|> parseForall
  <|> (do { id <- identifier;               return $ Var id })
  <?> "a type: Skip, T;T, !B, ?B, B, T->T, T-oT, (T,T), id, rec id.T, or forall id.t"

parsePair = do
  char '('
  t <- parseType
  char ','
  u <- parseType
  char ')'
  return $ Pair t u

parseRec = do
  rec
  --reserved "rec"
  -- space
  id <- identifier
  char '.'
  t <- parseType
  return $ Rec id t

parseForall = do
  forall
  -- space
  id <- identifier
  char '.'
  t <- parseType
  return $ Forall id t

parseInternalChoice = do
  reservedOp "+"
  char '{'
  a <- sepBy1 parseInternalPair comma
  -- a <- sepBy1 parseInternalPair (char ',')
  char '}'
  return $ InternalChoice $ Map.fromList a

parseExternalChoice = do
  reservedOp "&"
  char '{'
  a <- sepBy1 parseInternalPair comma
  char '}'
  return $ ExternalChoice $ Map.fromList a

parseDataType = do
  char '['
  a <- sepBy1 parseInternalPair comma
  char ']'
  return $ Datatype $ Map.fromList a

-- TODO: parens inside ?
-- read "+{(i : Int), b : Bool}" :: Type

parseInternalPair = do
  id <- identifier
  char ':'
  ptype <- parseType
  return (id,ptype)
