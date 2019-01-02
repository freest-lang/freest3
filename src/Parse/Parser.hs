{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Parse.Parser
(
  mainProgram
) where

import qualified Data.Map.Strict as Map
import           Parse.Lexer
import           Parse.TypeParser
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Text.Parsec (Parsec (..), modifyState)
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec
import           Validation.TypingState
import           Data.List

-- PARSER
type ParserOut = (VarEnv, ExpEnv, ConstructorEnv, KindEnv)
type CFSTParser = Parsec String ParserOut ParserOut
type CFSTSubParser = Parsec String ParserOut

mainProgram :: FilePath -> VarEnv -> IO (Either ParseError ParserOut)
mainProgram filepath venv = do
  file <- readFile filepath
  return $ parseWith filepath  program (venv, Map.empty, Map.empty, Map.empty) file

parseWith :: FilePath -> CFSTParser -> ParserOut -> String -> Either ParseError ParserOut
parseWith filepath p pout = runParser p pout ("CFST Error in " ++ filepath)

program :: CFSTParser
program = do
  whiteSpace  
  many1 (try parseTypeSignature <|> try parseFunction <|> parseDataType)
  eof
  s <- getState
  return s

-- MAIN PARSER COMBINATORS
-- Responsible for parsing Type Signatures, Expression Declarations and DataTypes

parseTypeSignature :: CFSTSubParser ()
parseTypeSignature = do
  pos <- getPosition
  id <- lowerIdentifier
  s <- getState
  checkDup (getVEnv s) id ("Duplicate type signatures for '" ++ id ++ "'") pos
  colon
  colon
--  optional $ try $ parseTypeBinding
  t <- (do {t1 <- parseType; return (TypeScheme [] t1)} <|> parseTypeScheme)
  modifyState (changeVEnv id t)

parseTypeScheme :: CFSTSubParser TypeScheme
parseTypeScheme = do
-- reserved "forall"
  forall
  bindings <- commaSep1 parseBindings
  reserved "=>"  
  t <- parseType
  return $ TypeScheme bindings t

parseBindings = do
  id <- identifier
  k <- option (Kind Session Un) parseVarBind
  return $ Bind id k

parseFunction :: CFSTSubParser ()
parseFunction = do
  pos <- getPosition
  id <- try lowerIdentifier
  ids <- (many lowerIdentifier)
  reservedOp "="
  e <- parseExpression
  modifyState (changeEEnv id (ids, e)) 
  
-- parseTypeDecl :: [String] -> Parser ([String], (TermVar, Type))
-- parseTypeDecl err = do
--   reserved "type"
--   c <- constructor
--   reservedOp "="
--   t <- parseType
--   return (err, (c, t))


parseDataType :: CFSTSubParser ()
parseDataType = do
  reserved "data"
  pos <- getPosition
  c <- constructor
  s <- getState
  checkDup (getKEnv s) c ("Multiple declarations of '" ++ c ++ "'") pos
  k <- option (Kind Functional Un) parseVarBind
  reservedOp "="
  ts <- sepBy1 parseTypeComponents (lexeme (char '|'))
  checkDupComp (map fst ts) "Multiple declarations of '" pos
  let bindingList = types c ts
  mapM (\(tc, v) -> modifyState (changeCEnv tc (TypeScheme [] v))) bindingList
  modifyState (changeKEnv c k)
  modifyState (changeVEnv c (TypeScheme [] (Datatype (Map.fromList bindingList))))
  return ()

  where
    types :: TypeVar -> [(TypeVar, [Type])] -> [(TypeVar, Type)]
    types tv = foldl (\acc (k, ts) -> acc ++ [(k, typeToFun tv ts)]) [] 

    typeToFun :: TypeVar -> [Type] -> Type
    typeToFun c [] = (Var c)
    typeToFun c (x:xs) = Fun Un x (typeToFun c xs)

    checkDupComp :: [TypeVar] -> String -> SourcePos -> CFSTSubParser ()
    checkDupComp bs msg pos
      | length bs == length (nub bs) = return ()
      | otherwise = fail $ msg ++ show (nub(intersect bs (nub bs))) ++ "'" ++ position 
      where position = " (line " ++ show (sourceLine pos) ++
                       ", column " ++ show (sourceColumn pos) ++ ")"

-- -- Checking for duplicate declarations
-- checkDup :: Ord k => Map.Map k a -> k -> [Char] -> SourcePos -> CFSTSubParser ()
-- checkDup env id msg pos
--   | Map.member id env = fail $ msg ++ position
--   | otherwise         = return ()
--   where position = " (line " ++ show (sourceLine pos) ++
--                    ", column " ++ show (sourceColumn pos) ++ ")"


parseTypeComponents :: CFSTSubParser (TypeVar, [Type])
parseTypeComponents = do
  pos <- getPosition
  c <- try constructor
  s <- getState
  checkDup (getCEnv s) c ("Multiple declarations of '" ++ c ++ "'") pos
  ts <- manyTill (try parseType) (try untilParser)
  return (c, ts)

untilParser :: CFSTSubParser ()
untilParser = do
      lookAhead $ try $ parseFunction
  <|> (do {lookAhead $ try parseDataType})
  <|> (do {lookAhead $ try parseTypeSignature})
  <|> (do {lookAhead(try $ char '|'); return ()})
  <|> (do {lookAhead(try eof)})


-- PARSING EXPRESSIONS

-- Parses Apps
-- Builds a table that defines the priority and the associativity of each kind of App
table =
  [
    [ prefixOp "-" (App (0,0) (Variable (0,0) "negate"))
    ]
  ,
    [ binOp "*" (convertApp "(*)") AssocLeft
    , binOp "/" (convertApp "(/)") AssocLeft
    ]
  , [ binOp "+" (convertApp "(+)") AssocLeft
    , binOp "-" (convertApp "(-)") AssocLeft
    , binOp "mod" (convertApp "mod") AssocRight
    , binOp "rem" (convertApp "rem") AssocRight
    ]
  , [ binOp "&&" (convertApp "(&&)") AssocLeft
    , binOp "==" (convertApp "(==)") AssocLeft
    , prefixOp "not" (App (0,0) (Variable (0,0) "not"))
    , binOp "||" (convertApp "(||)") AssocLeft
    , binOp "<" (convertApp "(<)") AssocLeft
    , binOp ">" (convertApp "(>)") AssocLeft
    , binOp "<=" (convertApp "(<=)") AssocLeft
    , binOp ">=" (convertApp "(>=)") AssocLeft
    ]
  ]

-- Converts a binary App in an ternary application with an operator
convertApp :: TermVar -> Expression -> Expression -> Expression
convertApp op e1 e2 = (App (0,0) (App (0,0) (Variable (0,0) op) e1) e2)

binOp name fun assoc =  Infix (do reservedOp name; return fun) assoc
prefixOp name fun =  Prefix (do reservedOp name; return fun)

-- binary name fun assoc = Infix (do reserved name; return fun) assoc
-- prefix name fun =  Prefix (do reserved name; return fun)

-- Parses an expression
parseExpression :: CFSTSubParser Expression
parseExpression =
  buildExpressionParser table (parseFunApp <|> constructApp <|> parseExpr)
  
parseExpr :: CFSTSubParser Expression
parseExpr =
      try (parens parseExpression)
  <|> parseBasic
  <|> parseConditional
  <|> parsePair
  <|> parseUnLet
  <|> parseLet
  <|> parseNew
  <|> parseSend
  <|> parseReceive
  <|> parseSelect
  <|> parseFork
  <|> parseCase
  <|> parseMatch
  <|> parseTypeApp
  <|> parseVariable
  <|> parseConstructor
  <?> "an expression"


-- Parse Basic Types (int, bool, char and unit)
parseBasic :: CFSTSubParser Expression
parseBasic =
      (do pos <- getPosition; c <- apostrophe anyChar; return $ Character (posPair pos) c)
  <|> parseBool
  <|> parseInteger
  <|> (do  pos <- getPosition; reserved "()"; return $ Unit (posPair pos))
  <?> "basic type"

parseInteger :: CFSTSubParser Expression
parseInteger = do
  pos <- getPosition
  i <- natural
  return $ Integer (posPair pos) (fromInteger i)

parseBool :: CFSTSubParser Expression
parseBool =
      (do pos <- getPosition; reserved "True"; return $ Boolean (posPair pos) True)
  <|> (do pos <- getPosition; reserved "False"; return $ Boolean (posPair pos) False)
  <?> "a boolean value"
-- Parse Variables

parseVariable :: CFSTSubParser Expression
parseVariable =
  try $ do
    pos <- getPosition
    id <- lowerIdentifier
    notFollowedBy (do {colon;colon})
    return $ Variable (posPair pos) id

parseUnLet :: CFSTSubParser Expression
parseUnLet = try $ do
  reserved "let"
  pos <- getPosition
  id <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ UnLet (posPair pos) id e1 e2

-- Parse Pairs (Pair and let)
parsePair :: CFSTSubParser Expression
parsePair =
  try $ parens $ do  
    pos <- getPosition
    e1 <- parseExpression
    comma
    e2 <- parseExpression
    return $ Pair (posPair pos) e1 e2

parseLet :: CFSTSubParser Expression
parseLet = try $ do
  reserved "let"
  pos <- getPosition
  id1 <- lowerIdentifier
  comma
  id2 <- lowerIdentifier
  reservedOp "="
  e1 <- parseExpression
  reserved "in"
  e2 <- parseExpression
  return $ BinLet (posPair pos) id1 id2 e1 e2

-- Parse Conditional (if then else)
parseConditional :: CFSTSubParser Expression
parseConditional = do
  reserved "if"
  pos <- getPosition
  e1 <- parseExpression
  reserved "then"
  e2 <- parseExpression
  reserved "else"
  e3 <- parseExpression
--   error $ show e1
  return $ Conditional (posPair pos) e1 e2 e3

-- Parse Session Types (new, send, receive and select)
parseNew :: CFSTSubParser Expression
parseNew = do
  reserved "new"
  pos <- getPosition
  t <- parseType
  return $ New (posPair pos) t

parseSend :: CFSTSubParser Expression
parseSend = do
  reserved "send"
  pos <- getPosition
  e1 <- parseExpr
  e2 <- parseExpr
  return $ Send (posPair pos) e1 e2

parseReceive :: CFSTSubParser Expression
parseReceive = do
  reserved "receive"
  pos <- getPosition
  e <- parseExpr
  return $ Receive (posPair pos) e

parseSelect :: CFSTSubParser Expression
parseSelect = try $ do
  reserved "select"
  pos <- getPosition
  c <- constructor
--  notFollowedBy (constructApp <|> parseConstructor)
  e <- parseExpr
  return $ Select (posPair pos) c e

-- Parse Fork
parseFork :: CFSTSubParser Expression
parseFork = do
  reserved "fork"
  pos <- getPosition
  e <- parseExpression
  return $ Fork (posPair pos) e
  
-- Parse Datatypes
-- parseValue
parseMatch :: CFSTSubParser Expression
parseMatch = do
  reserved "match"
  -- t <- many1 anyChar
  -- error $ show t
  pos <- getPosition
  e <- parseExpression
  reserved "with"
  v <- many1 parseMatchValues
  return $ Match (posPair pos) e (Map.fromList v)

-- parseMatchOption = do
--   c <- constructor
--   id <- lowerIdentifier
--   reservedOp "->"
--   return (c, id)

parseMatchValues :: CFSTSubParser (String, (String, Expression))
parseMatchValues = do
--  (c, id) <- parseMatchOption
  c <- constructor
  id <- lowerIdentifier
  reservedOp "->"
  e <- try parseExpr
  return $ (c, (id, e))

parseCase :: CFSTSubParser Expression
parseCase = do
  reserved "case"
  pos <- getPosition
  e <- parseExpr
  reserved "of"
  v <- many1 parseCaseValues
  return $ Case (posPair pos) e (Map.fromList v)

parseCatchOption = do
  c <- constructor
  ids <- (many lowerIdentifier)
  reservedOp "->"
  return (c, ids)

parseCaseValues :: CFSTSubParser (String, ([String], Expression))
parseCaseValues = do
  (c, ids) <- parseCatchOption
  e <- parseExpression
  return $ (c, (ids, e))

parseConstructor :: CFSTSubParser Expression
parseConstructor = do
  c <- constructor
  pos <- getPosition
  return $ Constructor (posPair pos) c

parseFunApp :: CFSTSubParser Expression
parseFunApp = try $ do
  pos <- getPosition
  c <- parseVariable
--  notFollowedBy constructor
  e <- many1 parseExpr
-- e <- manyTill parseExpression untilParser1
  let p = posPair pos
  return $ foldl (apply p) (App p c (head e)) (tail e)
  where
    apply p acc e = App p acc e

parseTypeApp :: CFSTSubParser Expression
parseTypeApp = try $ do
  pos <- getPosition
  c <- parseVariable
  ts <- squares $ commaSep parseType
  e <- many parseExpr  -- (try $ parens parseExpression)
--  e <- manyTill parseExpression untilParser1  -- (try $ parens parseExpression)
  let p = posPair pos
  return $ foldl (apply p) (TypeApp p c ts) e
  where
     apply p acc e = App p acc e

-- untilParser1 :: CFSTSubParser ()
-- untilParser1 = do
--           lookAhead $ try $ parseMatchOption >> return ()
--           lookAhead $ try $ parseCatchOption >> return ()
--   <|> do {lookAhead $ try (reserved "in") >> return ()}
--   <|> do {lookAhead $ try $ parseFunction}
--   <|> (do {lookAhead $ try parseDataType})
--   <|> (do {lookAhead $ try parseTypeSignature})
--   <|> do {lookAhead $ try eof >> return ()}
  

constructApp :: CFSTSubParser Expression
constructApp = try $ do 
  pos <- getPosition
  c <- parseConstructor
  e <- many1 parseExpr
  let p = posPair pos
  return $ foldl (apply p) (App p c (head e)) (tail e)
  where
    apply p acc e = App p acc e

-- Helper functions to manage ParserOut
-- Get and insert elements for each element of ParserOut n-tuple

changeVEnv :: TypeVar -> TypeScheme -> ParserOut -> ParserOut
changeVEnv k v (m1,m2,m3,m4) = (Map.insert k v m1, m2, m3, m4)

getVEnv :: ParserOut -> VarEnv
getVEnv (m1,_,_,_) = m1

changeEEnv :: TermVar -> (Params, Expression) -> ParserOut -> ParserOut
changeEEnv k v (m1,m2,m3,m4) = (m1, Map.insert k v m2, m3, m4)

changeCEnv :: TermVar -> TypeScheme -> ParserOut -> ParserOut
changeCEnv k v (m1,m2,m3,m4) = (m1, m2, Map.insert k v m3, m4)

getCEnv :: ParserOut -> ConstructorEnv
getCEnv (_,_,m3,_) = m3

changeKEnv :: TermVar -> Kind -> ParserOut -> ParserOut
changeKEnv k v (m1,m2,m3,m4) = (m1, m2, m3, Map.insert k v m4)

getKEnv :: ParserOut -> KindEnv
getKEnv (_,_,_,m4) = m4

-- Checking for duplicate declarations
checkDup :: Ord k => Map.Map k a -> k -> [Char] -> SourcePos -> CFSTSubParser ()
checkDup env id msg pos
  | Map.member id env = fail $ msg ++ position
  | otherwise         = return ()
  where position = " (line " ++ show (sourceLine pos) ++
                   ", column " ++ show (sourceColumn pos) ++ ")"


posPair :: SourcePos -> (Int, Int)
posPair pos = (sourceLine pos, sourceColumn pos)

-- Read instance for type schemes

instance Read TypeScheme where
  readsPrec _ s =
    case parserTScheme s of
      Right b -> [(b, "")]
      Left m  -> error "TypeScheme parse error"
    where
      parserTScheme s =
        runParser parseTypeScheme (Map.empty, Map.empty, Map.empty, Map.empty) "" s


--TODO: remove (test purposes)
--run = mainProgram path Map.empty
--path = "src/test.hs"
--path = "test/Programs/ValidTests/multDivPrecedence/multDivPrecedence.hs"
--path = "/home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/sendTree/sendTree.hs"

