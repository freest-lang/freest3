{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where


import           Syntax.Base 
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
  $white*$eol+                  { \p s -> TokenNL (internalPos p s) }
  $white+                       ;
  $white*"--".*                 ;
  @blockComment                 ;
  module                        { \p s -> TokenModule(internalPos p s) }
  where                         { \p s -> TokenWhere (internalPos p s) }
  import                        { \p s -> TokenImport (internalPos p s) }
  ("->"|→|"*->"|"*→")           { \p s -> TokenUnArrow (internalPos p s) } 
  ("1->"|"1→")                  { \p s -> TokenLinArrow (internalPos p s) }
  ("\"|λ)                       { \p s -> TokenLambda (internalPos p s) } --" -- HACK: stop syntax highlighting going wild
  ("\\"|Λ)                      { \p s -> TokenUpperLambda (internalPos p s) }
  ("=>"|⇒)                      { \p s -> TokenFArrow (internalPos p s) }
  "@"                           { \p s -> TokenAt (internalPos p s)}
  "("				{ \p s -> TokenLParen (internalPos p s) }
  ")"				{ \p s -> TokenRParen (internalPos p s) }
  "["				{ \p s -> TokenLBracket (internalPos p s) }
  "]"			        { \p s -> TokenRBracket (internalPos p s) }
  "{"				{ \p s -> TokenLBrace (internalPos p s) }
  "}"			        { \p s -> TokenRBrace (internalPos p s) }
  ","				{ \p s -> TokenComma (internalPos p s) }
  ":"                           { \p s -> TokenColon (internalPos p s) }
  "::"                           { \p s -> TokenDoubleColon (internalPos p s) }
  ";"	       	      	  	{ \p s -> TokenSemi (internalPos p s) }
  "!"                           { \p s -> TokenMOut (internalPos p s) }
  "?"				{ \p s -> TokenMIn (internalPos p s) }
  "&"				{ \p s -> TokenAmpersand (internalPos p s) }
  "|>"				{ \p s -> TokenPipeOp (internalPos p s) }
  "."                           { \p s -> TokenDot (internalPos p s) }
  "="                           { \p s -> TokenEq (internalPos p s) }
  "|"                           { \p s -> TokenPipe (internalPos p s) }
-- Operators
  "+"			        { \p s -> TokenPlus (internalPos p s) }
  "-"                           { \p s -> TokenMinus (internalPos p s) }
  "*"				{ \p s -> TokenTimes (internalPos p s) }
  "^"				{ \p s -> TokenRaise (internalPos p s) }
  "_"				{ \p s -> TokenWild (internalPos p s) }
  ">"  	          		{ \p s -> TokenCmp (internalPos p s) "(>)" }
  "<"  	          		{ \p s -> TokenCmp (internalPos p s) "(<)" }
  ">="  		        { \p s -> TokenCmp (internalPos p s) "(>=)" }
  "<="  		        { \p s -> TokenCmp (internalPos p s) "(<=)" }
  "=="  		        { \p s -> TokenCmp (internalPos p s) "(==)" }
  "/="  		        { \p s -> TokenCmp (internalPos p s) "(/=)" }
  ("&&"|∧)  		        { \p s -> TokenConjunction (internalPos p s) }
  ("||"|∨)  		        { \p s -> TokenDisjunction (internalPos p s) }
  "++"      { \p s -> TokenAppend (internalPos p s)}
  "/"  		                { \p s -> TokenDiv (internalPos p s) }
  "$"  		                { \p s -> TokenDollar (internalPos p s) }
-- Kinds
  "*S"                          { \p s -> TokenUnS (internalPos p s) }
  "1S"                          { \p s -> TokenLinS (internalPos p s) }
  "*T"                          { \p s -> TokenUnT (internalPos p s) }
  "1T"                          { \p s -> TokenLinT (internalPos p s) }
  "1A"                          { \p s -> TokenLinA (internalPos p s) }
  "*A"                          { \p s -> TokenUnA (internalPos p s) } -- TODO: remove later
-- Basic types
  Int			        { \p s -> TokenIntT (internalPos p s) }
  Char				{ \p s -> TokenCharT (internalPos p s) }
  String			{ \p s -> TokenStringT (internalPos p s) }
  Skip				{ \p s -> TokenSkip (internalPos p s) }
  End			  	{ \p s -> TokenEnd (internalPos p s) }
-- Keywords
  (rec|μ)                       { \p s -> TokenRec (internalPos p s) }
  let                           { \p s -> TokenLet (internalPos p s) }
  in                            { \p s -> TokenIn (internalPos p s) }
  data                          { \p s -> TokenData (internalPos p s) }
  type                          { \p s -> TokenType (internalPos p s) }
  otherwise			{ \p s -> TokenOtherwise (internalPos p s) }
  if				{ \p s -> TokenIf (internalPos p s) }
  then				{ \p s -> TokenThen (internalPos p s) }
  else				{ \p s -> TokenElse (internalPos p s) }
--  new				{ \p s -> TokenNew (internalPos p s) }
  select		        { \p s -> TokenSelect (internalPos p s) }
  match				{ \p s -> TokenMatch (internalPos p s) }
  with				{ \p s -> TokenWith (internalPos p s) }
  case				{ \p s -> TokenCase (internalPos p s) }
  of				{ \p s -> TokenOf (internalPos p s) }
  (forall|∀)                    { \p s -> TokenForall (internalPos p s) }
  dualof			{ \p s -> TokenDualof (internalPos p s) }
-- Values
  \(\)				{ \p s -> TokenUnit (internalPos p s) }
  (0+|[1-9]$digit*)      	{ \p s -> TokenInt (internalPos p s) (read s) }
  @char				{ \p s -> TokenChar (internalPos p s) (read s) }
  @stringLiteral		{ \p s -> TokenString (internalPos p s) (read s) }
-- Identifiers
  @lowerId                      { \p s -> TokenLowerId (internalPos p s) s }
  @upperId                      { \p s -> TokenUpperId (internalPos p s) s }

{

data Token =
    TokenNL Span
  | TokenIntT Span
  | TokenCharT Span
  | TokenStringT Span
  | TokenUnit Span
  | TokenUnArrow Span
  | TokenLinArrow Span
  | TokenLambda Span
  | TokenUpperLambda Span
  | TokenAt Span
  | TokenLParen Span
  | TokenRParen Span
  | TokenLBracket Span
  | TokenRBracket Span
  | TokenComma Span
  | TokenSkip Span
  | TokenEnd Span
  | TokenColon Span
  | TokenDoubleColon Span
  | TokenUpperId Span String
  | TokenSemi Span
  | TokenMOut Span
  | TokenMIn Span
  | TokenLBrace Span
  | TokenRBrace Span
  | TokenAmpersand Span
  | TokenPipeOp Span
  | TokenPlus Span
  | TokenRec Span
  | TokenDot Span
  | TokenLowerId Span String
  | TokenUnS Span
  | TokenLinS Span
  | TokenUnT Span
  | TokenLinT Span
  -- | TokenUnM Span
  -- | TokenLinM Span
  | TokenLinA Span
  | TokenUnA Span
  | TokenInt Span Int
  | TokenChar Span Char
  | TokenString Span String
  | TokenLet Span
  | TokenIn Span
  | TokenEq Span
  | TokenData Span
  | TokenType Span
  | TokenPipe Span
  | TokenOtherwise Span
  | TokenIf Span
  | TokenThen Span
  | TokenElse Span
--  | TokenNew Span
--  | TokenSend Span
--  | TokenReceive Span
  | TokenSelect Span
  | TokenMatch Span
  | TokenWith Span
--  | TokenFork Span
  | TokenCase Span
  | TokenOf Span
  | TokenForall Span
  | TokenDualof Span
  | TokenFArrow Span
  | TokenMinus Span
  | TokenTimes Span
  | TokenRaise Span
  | TokenWild Span
  | TokenLT Span
  | TokenGT Span
  | TokenCmp Span String
  | TokenConjunction Span
  | TokenDisjunction Span
  | TokenAppend Span
  | TokenDiv Span
  | TokenDollar Span
  | TokenModule Span
  | TokenWhere Span
  | TokenImport Span

instance Show Token where
  show (TokenNL _) = "\\n"
  show (TokenIntT _) = "Int"
  show (TokenCharT _) = "Char"
  show (TokenUnit _) = "()"
  show (TokenStringT _) = "String"
  show (TokenUnArrow _) = "->"
  show (TokenLinArrow _) = "1->"
  show (TokenLambda _) = "λ"
  show (TokenUpperLambda _) = "Λ"
  show (TokenAt _) = "@"
  show (TokenLParen _) = "("
  show (TokenRParen _) = ")"
  show (TokenLBracket _) = "["
  show (TokenRBracket _) = "]"
  show (TokenComma _) = ","
  show (TokenSkip _) = "Skip"
  show (TokenEnd _) = "End"
  show (TokenColon _) = ":"
  show (TokenDoubleColon _) = "::"
  show (TokenUpperId _ c) = "" ++ c
  show (TokenSemi _) = ";"
  show (TokenMOut _) = "!"
  show (TokenMIn _) = "?"
  show (TokenLBrace _) = "{"
  show (TokenRBrace _) = "}"
  show (TokenAmpersand _) = "&"
  show (TokenPipeOp _) = "|>"
  show (TokenPlus _) = "+"
  show (TokenRec _) = "rec"
  show (TokenDot _) = "."
  show (TokenLowerId _ s) = "" ++ s
  show (TokenUnS _) = "*S"
  show (TokenLinS _) = "1S"
  show (TokenUnT _) = "*T"
  show (TokenLinT _) = "1T"
  show (TokenUnA _) = "*A"
  show (TokenLinA _) = "1A"
  -- show (TokenUnM _) = "*M"
  -- show (TokenLinM _) = "1M"
  show (TokenInt _ i) = show i
  show (TokenChar _ c) = show c
  show (TokenString _ s) = s
  show (TokenLet _) = "let"
  show (TokenIn _) = "in"
  show (TokenEq _) = "="
  show (TokenData _) = "data"
  show (TokenType _) = "type"
  show (TokenPipe _) = "|"
  show (TokenOtherwise _) = "otherwise"
  show (TokenIf _) = "if"
  show (TokenThen _) = "then"
  show (TokenElse _) = "else"
--  show (TokenNew _) = "new"
--  show (TokenSend _) = "send"
--  show (TokenReceive _) = "receive"
  show (TokenSelect _) = "select"
--  show (TokenFork _) = "fork"
  show (TokenMatch _) = "match"
  show (TokenWith _) = "with"
  show (TokenCase _) = "case"
  show (TokenOf _) = "of"
  show (TokenForall _) = "forall"
  show (TokenDualof _) = "dualof"
  show (TokenFArrow _) = "=>"
  show (TokenMinus _) = "-"
  show (TokenTimes _) = "*"
  show (TokenRaise _) = "^"
  show (TokenWild _) = "_"
  show (TokenLT _) = "<"
  show (TokenGT _) = ">"
  show (TokenCmp _ s) = show s
  show (TokenConjunction _) = "&&"
  show (TokenDisjunction _) = "||"
  show (TokenAppend _) = "++"
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
          let p = internalPos pos input in -- TODO: is this correct????
          Left $ LexicalError (Span (startPos p) (endPos p) "" filename) (show $ head input)
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

internalPos :: AlexPosn -> String -> Span
internalPos (AlexPn _ l c) src = let p = (l, c) in Span p p src ""

-- TODO: proper spans?, proper filename
instance Located Token where
  getSpan (TokenNL p) = p
  getSpan (TokenIntT p) = p
  getSpan (TokenCharT p) = p
  getSpan (TokenUnit p) = p
  getSpan (TokenStringT p) = p
  getSpan (TokenUnArrow p) = p
  getSpan (TokenLinArrow p) = p
  getSpan (TokenLambda p) = p
  getSpan (TokenUpperLambda p) = p
  getSpan (TokenAt p) = p
  getSpan (TokenLParen p) = p
  getSpan (TokenRParen p) = p
  getSpan (TokenLBracket p) = p
  getSpan (TokenRBracket p) = p
  getSpan (TokenComma p) = p
  getSpan (TokenSkip p) = p
  getSpan (TokenEnd p) = p
  getSpan (TokenColon p) = p
  getSpan (TokenDoubleColon p) = p
  getSpan (TokenUpperId p _) = p
  getSpan (TokenSemi p) = p
  getSpan (TokenMOut p) = p
  getSpan (TokenMIn p) = p
  getSpan (TokenLBrace p) = p
  getSpan (TokenRBrace p) = p
  getSpan (TokenAmpersand p) = p
  getSpan (TokenPipeOp p) = p
  getSpan (TokenPlus p) = p
  getSpan (TokenRec p) = p
  getSpan (TokenDot p) = p
  getSpan (TokenLowerId p _) = p
  getSpan (TokenUnS p) = p
  getSpan (TokenLinS p) = p
  getSpan (TokenUnT p) = p
  getSpan (TokenLinT p) = p
  getSpan (TokenLinA p) = p
  getSpan (TokenUnA p) = p
  -- getSpan (TokenLinM p) = p
  -- getSpan (TokenUnM p) = p
  getSpan (TokenInt p _) = p
  getSpan (TokenChar p _) = p
  getSpan (TokenString p _) = p
  getSpan (TokenLet p) = p
  getSpan (TokenIn p) = p
  getSpan (TokenEq p) = p
  getSpan (TokenData p) = p
  getSpan (TokenType p) = p
  getSpan (TokenPipe p) = p
--  getSpan (TokenNew p) = p
--  getSpan (TokenSend p) = p
--  getSpan (TokenReceive p) = p
  getSpan (TokenSelect p) = p
--  getSpan (TokenFork p) = p
  getSpan (TokenMatch p) = p
  getSpan (TokenCase p) = p
  getSpan (TokenForall p) = p
  getSpan (TokenMinus p) = p
  getSpan (TokenTimes p) = p
  getSpan (TokenRaise p) = p
  getSpan (TokenLT p) = p
  getSpan (TokenGT p) = p
  getSpan (TokenWild p) = p
  getSpan (TokenCmp p _) = p
  getSpan (TokenOtherwise p) = p
  getSpan (TokenIf p) = p
  getSpan (TokenThen p) = p
  getSpan (TokenElse p) = p
  getSpan (TokenWith p) = p
  getSpan (TokenOf p) = p
  getSpan (TokenDualof p) = p
  getSpan (TokenFArrow p) = p
  getSpan (TokenConjunction p) = p
  getSpan (TokenDisjunction p) = p
  getSpan (TokenAppend p) = p
  getSpan (TokenDiv p) = p
  getSpan (TokenDollar p) = p
  getSpan (TokenModule p) = p
  getSpan (TokenWhere p) = p
  getSpan (TokenImport p) = p
--  pos t = error $ show t


getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenCmp _ x) = x

}
