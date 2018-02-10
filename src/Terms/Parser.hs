{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Terms.Parser (mainProgram) where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Types.Types
import Types.Kinding
import Types.Parser
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Either
-- import System.Directory
-- import qualified Control.Applicative as ca


--TODO: review
type Args = [String]

data Program =
    Empty
  | TypeDecl Id Type
  | FunDecl Id Args Expression
  -- | FunDecl Id Type Expression
  deriving Show


type TermVar = String
type TypeVar = String

type TermEnv = Map.Map TermVar (Type, Expression)
type TypeEnv = Map.Map TypeVar Type

data Expression =
    BasicTerm BasicType
  | IntApp Expression Expression
  | BoolApp Expression Expression
  | Elim Expression Expression
  deriving Show


-- LEXER
lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser
        (haskellDef
        {
        Token.reservedOpNames = ["=","+","-","*","/", "mod", "rev", "&&", "||", "not"],
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

-- comma      = Token.comma lexer
-- -- semi = Token.semi lexer
-- dot = Token.dot lexer
-- braces = Token.braces lexer
-- squares = Token.squares lexer
-- send = reserved "send"

-- PARSER


-- type ProgramOut = ([Program], [Program])

-- parserProgram :: String -> Either ParseError [Program]
-- parserProgram = parse mainParser "Context-free Sessions (Parsing)"
mainProgram :: IO (Either ParseError (Map.Map Id Type, Map.Map Id Expression))
mainProgram = parseFromFile program "src/Terms/test.hs"
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

convertToMap :: Map.Map Id Type -> Map.Map Id Expression -> [Program] -> (Map.Map Id Type, Map.Map Id Expression)
convertToMap typeMap termMap []  = (typeMap,termMap)
convertToMap typeMap termMap (x:xs) =
  case x of
    (TypeDecl i t) -> convertToMap (Map.insert i t typeMap) termMap xs
    (FunDecl i _ e) -> convertToMap typeMap (Map.insert i e termMap) xs
    _ -> error "not a type decl"
  -- | FunDecl i Args Expression

-- parseProgram :: Parser Program
-- parseProgram =
--   choice [parseExpressionDecl, parseTypeDecl]
  --     (Text.Parsec.try parseTypeDecl)
  -- <|> (Text.Parsec.try parseExpressionDecl)
  -- <?> "Program error"

-- TODO: well kinded
-- parseTypeDecl = do
--   id <- identifier
--   colon
--   colon
--   t <- mainTypeParser
--   return $ TypeDecl id t

parseTypeDecl = do
  id <- identifier
  colon
  colon
  t <- mainTypeParser
  if isType t then
    return $ TypeDecl id t
  else
    error $ "Type t is not well kinded: " ++ show t
  -- case t of
  --   Right t' -> if isType t then (return $ TypeDecl id t') else error $ "Type "++ (show t') ++" not well kinded"
  --   Left m -> error $ "type parse error " ++ show m
  -- return $ TypeDecl id t


  -- Right t -> if isType t then [(t,"")] else error $ "Type "++ (show t) ++" not well kinded"
  -- Left m -> error $ "type parse error " ++ show m

parseExpressionDecl = do
  id <- identifier
  ids <- (many identifier)
  reservedOp "="
  e <- parseExpression
  return $ FunDecl id ids e

--TODO: mod and rev Associativity and precedence
--TODO: bool priority
--TODO: bool app one expr when applying not Operator

table = [ [binOp "*" IntApp AssocLeft, binOp "/" IntApp AssocLeft ]
        , [binOp "+" IntApp AssocLeft, binOp "-" IntApp AssocLeft,
            binary "mod" IntApp AssocLeft, binary "rev" IntApp AssocLeft ]
        , [binOp "&&" BoolApp AssocLeft, binOp "||" BoolApp AssocLeft
            --, prefix "not" BoolApp
          ]
        ]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

-- table = []
-- parseExpression = buildExpressionParser table (lexeme parseExpr)
parseExpression = buildExpressionParser table parseExpr


parseExpr =
  parseBasic

parseBasic =
      (do {integer; return $ BasicTerm IntType})
  <|> parseBool
  <|> (do {anyChar; return $ BasicTerm CharType})

parseBool =
      (do {reserved "True"; return $ BasicTerm BoolType})
  <|> (do {reserved "False"; return $ BasicTerm BoolType})


--
