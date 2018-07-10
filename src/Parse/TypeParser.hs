{-# LANGUAGE FlexibleContexts #-}
-- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.htm)

module Parse.TypeParser (
  parseType
, parseKind
, parseVarBind  
) where

import qualified Data.Map.Strict as Map
import           Parse.Lexer
import           Syntax.Kinds
import           Syntax.Types
import           Text.Parsec (Parsec (..))
import           Text.Parsec.Expr
import           Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Token
import           Text.ParserCombinators.Parsec


instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m  -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> [(t,"")]
    Left m -> error $ "type parse error " ++ show m


instance Read Kind where
  readsPrec _ s = parsecToReadsPrec parseKind 0 s
    -- case parserKind s of
    -- Right k -> [(k,"")]
    -- Left m -> error $ "kind error: \n" ++ show m ++ " , but got: " ++ show s

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = (,) <$> p <*> getInput

parsecToReadsPrec :: Parser a -> Int -> ReadS a
parsecToReadsPrec parsecParser prec s
    = case parse (withRemaining  (do {whiteSpace; parsecParser})) "" s of
        Left _ -> []
        Right result -> [result]


-- PARSE BASIC TYPES
--   IntType | CharType | BoolType | UnitType

parserBasic :: String -> Either ParseError BasicType
parserBasic = parse parseBasicType "Context-free Sessions (Basic types)"

parserKind :: String -> Either ParseError Kind
parserKind = parse parseKind "Context-free Sessions (Kind)"


parseBasicType :: Parsec String u BasicType
parseBasicType =
      (spaces >> reserved "Int"  >> spaces >> return IntType)
  <|> (spaces >> reserved "Char"  >> spaces  >> return CharType)
  <|> (spaces >> reserved "Bool"  >> spaces >> return BoolType)
  <|> (spaces >>  reserved "()" >> spaces  >> return UnitType)
  <?> "a basic type: Int, Char, Bool, or ()"

-- PARSE TYPES

parserType :: String -> Either ParseError Type
parserType = parse parseType "Context-free Sessions (Types)"

parseType :: Parsec String u Type
parseType = 
     do{
      whiteSpace
      ; ret <- typeExpr
--      ; eof
      ; return ret
  } <?> "a type: skip, T;T, ..., or ..."

typeExpr :: Parsec String u Type
typeExpr =  buildExpressionParser table parseTerm
        <?> "an expression"
         
table = [[binary ";" Semi AssocLeft ]
        , [binary "->" (Fun Un) AssocRight, binary "-o" (Fun Lin) AssocRight ]
        ]

binary name fun assoc = Infix  (do{ try (symbol name); return fun }) assoc
-- prefix name fun       = Prefix (do{ reservedOp name; return fun })

parseTerm :: Parsec String u Type
parseTerm =
  try (parens typeExpr)
  <|> (do {  skip ;                                return Skip })
  <|> (do { b <- parseBasicType;                   return $ Basic b })
  <|> (do { try (symbol "?"); b <- parseBasicType; return $ Message In b })  
  <|> (do { try (symbol "!"); b <- parseBasicType; return $ Message Out b })  
  <|> parens parsePair
  <|> parseExternalChoice
  <|> parseInternalChoice
  <|> squares parseDataType
  <|> parseRec
  <|> parseVar
  <?> "a type: Skip, T;T, !B, ?B, B, T->T, T-oT, (T,T), id, rec id.T, or forall id.t"

  
parseVar :: Parsec String u Type
parseVar = do
  id <- identifier
  return $ Var id

parsePair :: Parsec String u Type
parsePair = do
  t <- typeExpr
  comma
  u <- typeExpr
  return $ PairType t u

parseRec :: Parsec String u Type
parseRec = do
  rec
  id <- identifier
  k <- option (Kind Session Lin) parseVarBind
  dot
  t <- typeExpr
  return $ Rec (Bind id k) t
 
parseInternalChoice :: Parsec String u Type
parseInternalChoice = do
  reservedOp "+"
  a <- braces $ sepBy1 parseBind comma
  return $ Choice Internal (Map.fromList a)

parseExternalChoice :: Parsec String u Type
parseExternalChoice = do
  reservedOp "&"
  a <- braces $ sepBy1 parseBind comma
  return $ Choice External (Map.fromList a)

parseDataType :: Parsec String u Type
parseDataType = do
  a <- sepBy1 parseBind comma
  return $ Datatype $ Map.fromList a

parseBind :: Parsec String u (TypeVar, Type)
parseBind = do
  id <- identifier
  colon
  ptype <- typeExpr
  return (id,ptype)


parseVarBind :: Parsec String u Kind
parseVarBind = do
  colon
  colon
  parseKind

parseKind :: Parsec String u Kind
parseKind = 
      (spaces >> reserved "SU" >> spaces >> return (Kind Session Un))
  <|> (spaces >> reserved "SL" >> spaces >> return (Kind Session Lin))
  <|> (spaces >> reserved "TU" >> spaces >> return (Kind Functional Un))
  <|> (spaces >> reserved "TL" >> spaces >> return (Kind Functional Lin))
  <?> "a kind: SU, SL, TU or TL"

