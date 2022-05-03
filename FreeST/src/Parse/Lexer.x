{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where


import           Syntax.Base (Pos(..), Position(..), Span(..), Spannable(..))
import           Util.Error (ErrorType(..))
}

%wrapper "posn"

$greek = [\880-\1023] -- # λ  -- forall not in range ([λ ∀])

$upperA  = [A-Z]
$upper = [$upperA$greek]

-- $lowerU  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$lowerA = [a-z]
$lower = [$lowerA$greek\_] --  $greek \_]

$letter = [$lower$upper$greek]

-- $unidigit  = \x03
$ascdigit = 0-9
$digit = [$ascdigit] -- $unidigit]

-- $ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol = [$ascsymbol] -- $unisymbol]

$alphaNumeric = [$letter$digit\_\']

$eol=[\n]

@char = \'(\\n|[\\.]|.) \'
@blockComment = "{-" (\-[^\}]|[^\-]|\n)* "-}"

-- # λ  -- forall not in range ([λ ∀])
$greekId = [λ ∀ Λ μ]

@lowerId = ($lower # $greekId) $alphaNumeric*
@upperId = ($upper # $greekId) $alphaNumeric*

@stringLiteral = \"(\\.|[^\"]|\n)*\"

tokens :-
  $white*$eol+                  { \p s -> TokenNL (internalPos p) }
  $white+                       ;
  $white*"--".*                 ;
  @blockComment                 ;
  module                        { \p s -> TokenModule(internalPos p) }
  where                         { \p s -> TokenWhere (internalPos p) }
  import                        { \p s -> TokenImport (internalPos p) }
  ("->"|→)			{ \p s -> TokenUnArrow (internalPos p) }
  ("-o"|⊸)                      { \p s -> TokenLinArrow (internalPos p) }
  ("\"|λ)                       { \p s -> TokenLambda (internalPos p) }
  ("\\"|Λ)                      { \p s -> TokenUpperLambda (internalPos p) }
  ("=>"|⇒)                      { \p s -> TokenFArrow (internalPos p) }
  "("				{ \p s -> TokenLParen (internalPos p) }
  ")"				{ \p s -> TokenRParen (internalPos p) }
  "["				{ \p s -> TokenLBracket (internalPos p) }
  "]"			        { \p s -> TokenRBracket (internalPos p) }
  "{"                           { \p s -> TokenLBrace (internalPos p) }
  "}"			        { \p s -> TokenRBrace (internalPos p) }
  ","				{ \p s -> TokenComma (internalPos p) }
  ":"                           { \p s -> TokenColon (internalPos p) }
  ";"	       	      	  	{ \p s -> TokenSemi (internalPos p) }
  "!"                           { \p s -> TokenMOut (internalPos p) }
  "?"				{ \p s -> TokenMIn (internalPos p) }
  "&"				{ \p s -> TokenAmpersand (internalPos p) }
  "."                           { \p s -> TokenDot (internalPos p) }
  "="                           { \p s -> TokenEq (internalPos p) }
  "|"                           { \p s -> TokenPipe (internalPos p) }
-- Operators
  "+"			        { \p s -> TokenPlus (internalPos p) }
  "-"                           { \p s -> TokenMinus (internalPos p) }
  "*"				{ \p s -> TokenTimes (internalPos p) }
  "^"				{ \p s -> TokenRaise (internalPos p) }
  "_"				{ \p s -> TokenWild (internalPos p) }
  ">"  	          		{ \p s -> TokenCmp (internalPos p) "(>)" }
  "<"  	          		{ \p s -> TokenCmp (internalPos p) "(<)" }
  ">="  		        { \p s -> TokenCmp (internalPos p) "(>=)" }
  "<="  		        { \p s -> TokenCmp (internalPos p) "(<=)" }
  "=="  		        { \p s -> TokenCmp (internalPos p) "(==)" }
  "/="  		        { \p s -> TokenCmp (internalPos p) "(/=)" }
  ("&&"|∧)  		        { \p s -> TokenConjunction (internalPos p) }
  ("||"|∨)  		        { \p s -> TokenDisjunction (internalPos p) }
  "/"  		                { \p s -> TokenDiv (internalPos p) }
  "$"  		                { \p s -> TokenDollar (internalPos p) }
-- Kinds
  SU                            { \p s -> TokenSU (internalPos p) }
  SL                            { \p s -> TokenSL (internalPos p) }
  TU                            { \p s -> TokenTU (internalPos p) }
  TL                            { \p s -> TokenTL (internalPos p) }
  MU                            { \p s -> TokenMU (internalPos p) }
  ML                            { \p s -> TokenML (internalPos p) }
-- Basic types
  Int			        { \p s -> TokenIntT (internalPos p) }
  Char				{ \p s -> TokenCharT (internalPos p) }
  Bool				{ \p s -> TokenBoolT (internalPos p) }
  String			{ \p s -> TokenStringT (internalPos p) }
  Skip				{ \p s -> TokenSkip (internalPos p) }
-- Keywords
  (rec|μ)                       { \p s -> TokenRec (internalPos p) }
  let                           { \p s -> TokenLet (internalPos p) }
  in                            { \p s -> TokenIn (internalPos p) }
  data                          { \p s -> TokenData (internalPos p) }
  type                          { \p s -> TokenType (internalPos p) }
  if				{ \p s -> TokenIf (internalPos p) }
  then				{ \p s -> TokenThen (internalPos p) }
  else				{ \p s -> TokenElse (internalPos p) }
  new				{ \p s -> TokenNew (internalPos p) }
  select		        { \p s -> TokenSelect (internalPos p) }
  match				{ \p s -> TokenMatch (internalPos p) }
  with				{ \p s -> TokenWith (internalPos p) }
  case				{ \p s -> TokenCase (internalPos p) }
  of				{ \p s -> TokenOf (internalPos p) }
  (forall|∀)                    { \p s -> TokenForall (internalPos p) }
  dualof			{ \p s -> TokenDualof (internalPos p) }
-- Values
  \(\)				{ \p s -> TokenUnit (internalPos p) }
  (0+|[1-9]$digit*)      	{ \p s -> TokenInt (internalPos p) (read s) }
  (True|False) 	      	 	{ \p s -> TokenBool (internalPos p) (read s) }
  @char				{ \p s -> TokenChar (internalPos p) (read s) }
  @stringLiteral		{ \p s -> TokenString (internalPos p) (read s) }
-- Identifiers
  ("(+)"|"(-)"|"(*)"|"(/)"
  |"(^)"|"(>)"|"(<)"|"(>=)"
  |"(<=)"|"(==)"|"(/=)"
  |"(&&)"|"(||)")               { \p s -> TokenLowerId (internalPos p) s }
  @lowerId                      { \p s -> TokenLowerId (internalPos p) s }
  @upperId                      { \p s -> TokenUpperId (internalPos p) s }

{

data Token =
    TokenNL Pos
  | TokenIntT Pos
  | TokenCharT Pos
  | TokenBoolT Pos
  | TokenStringT Pos
  | TokenUnit Pos
  | TokenUnArrow Pos
  | TokenLinArrow Pos
  | TokenLambda Pos
  | TokenUpperLambda Pos
  | TokenLParen Pos
  | TokenRParen Pos
  | TokenLBracket Pos
  | TokenRBracket Pos
  | TokenComma Pos
  | TokenSkip Pos
  | TokenColon Pos
  | TokenUpperId Pos String
  | TokenSemi Pos
  | TokenMOut Pos
  | TokenMIn Pos
  | TokenLBrace Pos
  | TokenRBrace Pos
  | TokenAmpersand Pos
  | TokenPlus Pos
  | TokenRec Pos
  | TokenDot Pos
  | TokenLowerId Pos String
  | TokenSU Pos
  | TokenSL Pos
  | TokenTU Pos
  | TokenTL Pos
  | TokenMU Pos
  | TokenML Pos
  | TokenInt Pos Int
  | TokenChar Pos Char
  | TokenString Pos String
  | TokenBool Pos Bool
  | TokenLet Pos
  | TokenIn Pos
  | TokenEq Pos
  | TokenData Pos
  | TokenType Pos
  | TokenPipe Pos
  | TokenIf Pos
  | TokenThen Pos
  | TokenElse Pos
  | TokenNew Pos
--  | TokenSend Pos
--  | TokenReceive Pos
  | TokenSelect Pos
  | TokenMatch Pos
  | TokenWith Pos
--  | TokenFork Pos
  | TokenCase Pos
  | TokenOf Pos
  | TokenForall Pos
  | TokenDualof Pos
  | TokenFArrow Pos
  | TokenMinus Pos
  | TokenTimes Pos
  | TokenRaise Pos
  | TokenWild Pos
  | TokenLT Pos
  | TokenGT Pos
  | TokenCmp Pos String
  | TokenConjunction Pos
  | TokenDisjunction Pos
  | TokenDiv Pos
  | TokenDollar Pos
  | TokenModule Pos
  | TokenWhere Pos
  | TokenImport Pos

instance Show Token where
  show (TokenNL _) = "\\n"
  show (TokenIntT _) = "Int"
  show (TokenCharT _) = "Char"
  show (TokenBoolT _) = "Bool"
  show (TokenUnit _) = "()"
  show (TokenStringT _) = "String"
  show (TokenUnArrow _) = "->"
  show (TokenLinArrow _) = "-o"
  show (TokenLambda _) = "λ"
  show (TokenUpperLambda _) = "Λ"
  show (TokenLParen _) = "("
  show (TokenRParen _) = ")"
  show (TokenLBracket _) = "["
  show (TokenRBracket _) = "]"
  show (TokenComma _) = ","
  show (TokenSkip _) = "Skip"
  show (TokenColon _) = ":"
  show (TokenUpperId _ c) = "" ++ c
  show (TokenSemi _) = ";"
  show (TokenMOut _) = "!"
  show (TokenMIn _) = "?"
  show (TokenLBrace _) = "{"
  show (TokenRBrace _) = "}"
  show (TokenAmpersand _) = "&"
  show (TokenPlus _) = "+"
  show (TokenRec _) = "rec"
  show (TokenDot _) = "."
  show (TokenLowerId _ s) = "" ++ s
  show (TokenSU _) = "SU"
  show (TokenSL _) = "SL"
  show (TokenTU _) = "TU"
  show (TokenTL _) = "TL"
  show (TokenMU _) = "MU"
  show (TokenML _) = "ML"
  show (TokenInt _ i) = show i
  show (TokenChar _ c) = show c
  show (TokenBool _ b) = show b
  show (TokenString _ s) = s
  show (TokenLet _) = "let"
  show (TokenIn _) = "in"
  show (TokenEq _) = "="
  show (TokenData _) = "data"
  show (TokenType _) = "type"
  show (TokenPipe _) = "|"
  show (TokenIf _) = "if"
  show (TokenThen _) = "then"
  show (TokenElse _) = "else"
  show (TokenNew _) = "new"
--  show (TokenSend _) = "send"
--  show (TokenReceive _) = "receive"
  show (TokenSelect _) = "select"
--  show (TokenFork _) = "fork"
  show (TokenMatch _) = "match"
  show (TokenCase _) = "case"
  show (TokenForall _) = "forall"
  show (TokenMinus _) = "-"
  show (TokenTimes _) = "*"
  show (TokenRaise _) = "^"
  show (TokenLT _) = "<"
  show (TokenGT _) = ">"
  show (TokenWild _) = "_"
  show (TokenCmp _ s) = show s
  show (TokenOf _) = "of"
  show (TokenDualof _) = "dualof"
  show (TokenFArrow _) = "=>"
  show (TokenConjunction _) = "&&"
  show (TokenDisjunction _) = "||"
  show (TokenDiv _) = "/"
  show (TokenDollar _) = "$"
  show (TokenModule _) = "module"
  show (TokenWhere _)  = "where"
  show (TokenImport _)  = "import"

-- Trim newlines
scanTokens :: String -> FilePath -> Either ErrorType [Token] 
scanTokens input filename =
    case go (alexStartPos,'\n',[],input) of
      Right x -> Right $ trim x
      x -> x
  where
    go inp@(pos,_,_,input) =
      case alexScan inp 0 of
        AlexEOF -> Right []
        AlexError _ ->
          let p = internalPos pos in
          Left $ LexicalError (Span p p filename) (show $ head input)
        AlexSkip  inp' len     -> go inp'
        AlexToken inp' len act ->
          case go inp' of
            Right x -> Right $ act pos (take len input) : x
            x -> x

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL _ : ts) = trim' ts
    trim' ts = ts

-- POSITIONS

internalPos :: AlexPosn -> Pos
internalPos (AlexPn _ l c) = Pos l c

instance Position Token where
  pos (TokenNL p) = p
  pos (TokenIntT p) = p
  pos (TokenCharT p) = p
  pos (TokenBoolT p) = p
  pos (TokenUnit p) = p
  pos (TokenStringT p) = p
  pos (TokenUnArrow p) = p
  pos (TokenLinArrow p) = p
  pos (TokenLambda p) = p
  pos (TokenUpperLambda p) = p
  pos (TokenLParen p) = p
  pos (TokenRParen p) = p
  pos (TokenLBracket p) = p
  pos (TokenRBracket p) = p
  pos (TokenComma p) = p
  pos (TokenSkip p) = p
  pos (TokenColon p) = p
  pos (TokenUpperId p _) = p
  pos (TokenSemi p) = p
  pos (TokenMOut p) = p
  pos (TokenMIn p) = p
  pos (TokenLBrace p) = p
  pos (TokenRBrace p) = p
  pos (TokenAmpersand p) = p
  pos (TokenPlus p) = p
  pos (TokenRec p) = p
  pos (TokenDot p) = p
  pos (TokenLowerId p _) = p
  pos (TokenSU p) = p
  pos (TokenSL p) = p
  pos (TokenTU p) = p
  pos (TokenTL p) = p
  pos (TokenML p) = p
  pos (TokenMU p) = p
  pos (TokenInt p _) = p
  pos (TokenChar p _) = p
  pos (TokenBool p _) = p
  pos (TokenString p _) = p
  pos (TokenLet p) = p
  pos (TokenIn p) = p
  pos (TokenEq p) = p
  pos (TokenData p) = p
  pos (TokenType p) = p
  pos (TokenPipe p) = p
  pos (TokenNew p) = p
--  pos (TokenSend p) = p
--  pos (TokenReceive p) = p
  pos (TokenSelect p) = p
--  pos (TokenFork p) = p
  pos (TokenMatch p) = p
  pos (TokenCase p) = p
  pos (TokenForall p) = p
  pos (TokenMinus p) = p
  pos (TokenTimes p) = p
  pos (TokenRaise p) = p
  pos (TokenLT p) = p
  pos (TokenGT p) = p
  pos (TokenWild p) = p
  pos (TokenCmp p _) = p
  pos (TokenIf p) = p
  pos (TokenThen p) = p
  pos (TokenElse p) = p
  pos (TokenWith p) = p
  pos (TokenOf p) = p
  pos (TokenDualof p) = p
  pos (TokenFArrow p) = p
  pos (TokenConjunction p) = p
  pos (TokenDisjunction p) = p
  pos (TokenDiv p) = p
  pos (TokenDollar p) = p
  pos (TokenModule p) = p
  pos (TokenWhere p) = p
  pos (TokenImport p) = p
--  pos t = error $ show t

-- TODO: proper spans, proper filename
instance Spannable Token where
  span (TokenNL p) = Span p p ""
  span (TokenIntT p) = Span p p ""
  span (TokenCharT p) = Span p p ""
  span (TokenBoolT p) = Span p p ""
  span (TokenUnit p) = Span p p ""
  span (TokenStringT p) = Span p p ""
  span (TokenUnArrow p) = Span p p ""
  span (TokenLinArrow p) = Span p p ""
  span (TokenLambda p) = Span p p ""
  span (TokenUpperLambda p) = Span p p ""
  span (TokenLParen p) = Span p p ""
  span (TokenRParen p) = Span p p ""
  span (TokenLBracket p) = Span p p ""
  span (TokenRBracket p) = Span p p ""
  span (TokenComma p) = Span p p ""
  span (TokenSkip p) = Span p p ""
  span (TokenColon p) = Span p p ""
  span (TokenUpperId p _) = Span p p ""
  span (TokenSemi p) = Span p p ""
  span (TokenMOut p) = Span p p ""
  span (TokenMIn p) = Span p p ""
  span (TokenLBrace p) = Span p p ""
  span (TokenRBrace p) = Span p p ""
  span (TokenAmpersand p) = Span p p ""
  span (TokenPlus p) = Span p p ""
  span (TokenRec p) = Span p p ""
  span (TokenDot p) = Span p p ""
  span (TokenLowerId p _) = Span p p ""
  span (TokenSU p) = Span p p ""
  span (TokenSL p) = Span p p ""
  span (TokenTU p) = Span p p ""
  span (TokenTL p) = Span p p ""
  span (TokenML p) = Span p p ""
  span (TokenMU p) = Span p p ""
  span (TokenInt p _) = Span p p ""
  span (TokenChar p _) = Span p p ""
  span (TokenBool p _) = Span p p ""
  span (TokenString p _) = Span p p ""
  span (TokenLet p) = Span p p ""
  span (TokenIn p) = Span p p ""
  span (TokenEq p) = Span p p ""
  span (TokenData p) = Span p p ""
  span (TokenType p) = Span p p ""
  span (TokenPipe p) = Span p p ""
  span (TokenNew p) = Span p p ""
--  span (TokenSend p) = Span p p ""
--  span (TokenReceive p) = Span p p ""
  span (TokenSelect p) = Span p p ""
--  span (TokenFork p) = Span p p ""
  span (TokenMatch p) = Span p p ""
  span (TokenCase p) = Span p p ""
  span (TokenForall p) = Span p p ""
  span (TokenMinus p) = Span p p ""
  span (TokenTimes p) = Span p p ""
  span (TokenRaise p) = Span p p ""
  span (TokenLT p) = Span p p ""
  span (TokenGT p) = Span p p ""
  span (TokenWild p) = Span p p ""
  span (TokenCmp p _) = Span p p ""
  span (TokenIf p) = Span p p ""
  span (TokenThen p) = Span p p ""
  span (TokenElse p) = Span p p ""
  span (TokenWith p) = Span p p ""
  span (TokenOf p) = Span p p ""
  span (TokenDualof p) = Span p p ""
  span (TokenFArrow p) = Span p p ""
  span (TokenConjunction p) = Span p p ""
  span (TokenDisjunction p) = Span p p ""
  span (TokenDiv p) = Span p p ""
  span (TokenDollar p) = Span p p ""
  span (TokenModule p) = Span p p ""
  span (TokenWhere p) = Span p p ""
  span (TokenImport p) = Span p p ""
--  pos t = error $ show t


getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenCmp _ x) = x

}
