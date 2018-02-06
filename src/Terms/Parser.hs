{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this


import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Types.Types
import Types.Parser

-- type TypeDecl = String
-- type FunDecl = String
type Args = [String]

data Program =
    Empty
  | TypeDecl Id Type
  | FunDecl Id Args Expression
  -- | FunDecl Id Type Expression
  deriving Show

-- data Operation =
--     Mult Int Int
--   | Div Int Int
--   | Sum Int Int
--   | Sub Int Int

data Expression =
    BasicTerm BasicType
  | App Expression Expression
  | BoolApp Expression Expression
  | Elim Expression Expression
  deriving Show


-- The lexer
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

-- comma      = Token.comma lexer
-- -- semi = Token.semi lexer
-- dot = Token.dot lexer
-- braces = Token.braces lexer
-- squares = Token.squares lexer
-- send = reserved "send"

integer :: Parser Integer
integer = Token.integer lexer


mainParser :: Parser Program
mainParser =
    do{
      whiteSpace
      ; ret <- lexeme parseProgram
      ; eof
      ; return ret
  } <?> "error"

 -- Text.Parsec.try

parseProgram :: Parser Program
parseProgram =
      Text.Parsec.try parseTypeDecl
  <|> Text.Parsec.try parseExpressionDecl
  <?> "Program error"

parseTypeDecl = do
  id <- identifier
  colon
  colon
  t <- mainTypeParser
  return $ TypeDecl id t

parseExpressionDecl = do
  id <- identifier
  ids <- many identifier
  reservedOp "="
  e <- parseExpression
  return $ FunDecl id ids e

--TODO: mod and rev Associativity and precedence
--TODO: 2rev2mod2 -> Valid
--TODO: bool priority
--TODO: bool app one expr when applying not Operator

table = [ [binOp "*" App AssocLeft, binOp "/" App AssocLeft ]
        , [binOp "+" App AssocLeft, binOp "-" App AssocLeft,
            binary "mod" App AssocLeft, binary "rev" App AssocLeft ]
        , [binOp "&&" BoolApp AssocLeft, binOp "||" BoolApp AssocLeft
            --, prefix "not" BoolApp
          ]
        ]

binOp name fun assoc = Infix  (do{ reservedOp name; return fun }) assoc
binary name fun assoc = Infix  (do{ reserved name; return fun }) assoc
prefix name fun       = Prefix (do{ reserved name; return fun })

{-
[ [prefix "-" negate, prefix "+" id ]
, [postfix "++" (+1)]
, [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
, [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
]
-}

-- table = []
parseExpression = buildExpressionParser table (lexeme parseExpr)

parseExpr =
  parseBasic

parseBasic =
      (do {integer; return $ BasicTerm IntType})
  <|> parseBool
  <|> (do {anyChar; return $ BasicTerm CharType})

parseBool =
      (do {reserved "True"; return $ BasicTerm BoolType})
  <|> (do {reserved "False"; return $ BasicTerm BoolType})


run = parseTest mainParser
