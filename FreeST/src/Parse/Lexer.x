{
{-# OPTIONS_GHC -Wno-all #-}
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where


import           Syntax.Base 
import           Util.Error (ErrorType(..), internalError)
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

@numspc = _*         -- numeric spacer 

@decimal  = $digit(@numspc$digit)*
@exponent = [eE][\-\+]?@decimal

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
  mutual                        { \p s -> TokenMutual (internalPos p) }
  and                           { \p s -> TokenAnd (internalPos p) }
  ("->"|→|"*->"|"*→")           { \p s -> TokenUnArrow (internalPos p) }
  ("1->"|"1→")                  { \p s -> TokenLinArrow (internalPos p) }
  ("\"|λ)                       { \p s -> TokenLambda (internalPos p) }
  ("\\"|Λ)                      { \p s -> TokenUpperLambda (internalPos p) }
  ("=>"|⇒)                      { \p s -> TokenFArrow (internalPos p) }
  "@"                           { \p s -> TokenAt (internalPos p)}
  "("				{ \p s -> TokenLParen (internalPos p) }
  ")"				{ \p s -> TokenRParen (internalPos p) }
  "["				{ \p s -> TokenLBracket (internalPos p) }
  "]"			        { \p s -> TokenRBracket (internalPos p) }
  "{"				{ \p s -> TokenLBrace (internalPos p) }
  "}"			        { \p s -> TokenRBrace (internalPos p) }
  ","				{ \p s -> TokenComma (internalPos p) }
  ":"                           { \p s -> TokenColon (internalPos p) }
  "::"                           { \p s -> TokenDoubleColon (internalPos p) }
  ";"	       	      	  	{ \p s -> TokenSemi (internalPos p) }
  "!"                           { \p s -> TokenMOut (internalPos p) }
  "?"				{ \p s -> TokenMIn (internalPos p) }
  "&"				{ \p s -> TokenAmpersand (internalPos p) }
  "|>"				{ \p s -> TokenPipeOp (internalPos p) }
  "."                           { \p s -> TokenDot (internalPos p) }
  "="                           { \p s -> TokenEq (internalPos p) }
  "|"                           { \p s -> TokenPipe (internalPos p) }
-- Operators
  "+"			        { \p s -> TokenPlus (internalPos p) }
  "+."            { \p s -> TokenPlusF (internalPos p)}
  "+i"            { \p s -> TokenPlusI (internalPos p)}
  "-"                           { \p s -> TokenMinus (internalPos p) }
  "-."      {\p s -> TokenMinusDot (internalPos p)}
  "-i"      {\p s -> TokenMinusI (internalPos p)}
  "*"				{ \p s -> TokenTimes (internalPos p) }
  "*."      { \p s -> TokenTimesDot (internalPos p)}
  "*i"      { \p s -> TokenTimesI (internalPos p)}
  "^"				{ \p s -> TokenRaise (internalPos p) }
  "**"      { \p s -> TokenRaiseTimes (internalPos p)}
  "^i"      { \p s -> TokenRaiseI (internalPos p)}
  "_"				{ \p s -> TokenWild (internalPos p) }
  ">"  	          		{ \p s -> TokenCmp (internalPos p) "(>)" }
  "<"  	          		{ \p s -> TokenCmp (internalPos p) "(<)" }
  ">."                { \p s -> TokenCmp (internalPos p) "(>.)"}
  "<."                { \p s -> TokenCmp (internalPos p) "(<.)"}
  ">i"                { \p s -> TokenCmp (internalPos p) "(>i)"}
  "<i"                { \p s -> TokenCmp (internalPos p) "(<i)"}
  ">="  		        { \p s -> TokenCmp (internalPos p) "(>=)" }
  "<="  		        { \p s -> TokenCmp (internalPos p) "(<=)" }
  ">=."                { \p s -> TokenCmp (internalPos p) "(>=.)"}
  "<=."                { \p s -> TokenCmp (internalPos p) "(<=.)"}
  ">=i"                { \p s -> TokenCmp (internalPos p) "(>=i)"}
  "<=i"                { \p s -> TokenCmp (internalPos p) "(<=i)"}
  "=="  		        { \p s -> TokenCmp (internalPos p) "(==)" }
  "/="  		        { \p s -> TokenCmp (internalPos p) "(/=)" }
  "==i"  		        { \p s -> TokenCmp (internalPos p) "(==i)" }
  "/=i"  		        { \p s -> TokenCmp (internalPos p) "(/=i)" }
  ("&&"|∧)  		        { \p s -> TokenConjunction (internalPos p) }
  ("||"|∨)  		        { \p s -> TokenDisjunction (internalPos p) }
  "++"      { \p s -> TokenAppend (internalPos p)}
  "^^"      { \p s -> TokenAppendString (internalPos p)}
  "/"  		                { \p s -> TokenDiv (internalPos p) }
  "/."                    { \p s -> TokenDivDot (internalPos p)}
  "/i"                    { \p s -> TokenDivI (internalPos p)}
  "$"  		                { \p s -> TokenDollar (internalPos p) }
-- Kinds
  "*S"                          { \p s -> TokenUnS (internalPos p) }
  "1S"                          { \p s -> TokenLinS (internalPos p) }
  "*T"                          { \p s -> TokenUnT (internalPos p) }
  "1T"                          { \p s -> TokenLinT (internalPos p) }
  "1A"                          { \p s -> TokenLinA (internalPos p) }
  "*A"                          { \p s -> TokenUnA (internalPos p) } -- TODO: remove later
-- Basic types
  Int			        { \p s -> TokenIntT (internalPos p) }
  Float       { \p s -> TokenFloatT (internalPos p)}
  Integer { \p s -> TokenIntegerT (internalPos p)}
  Char				{ \p s -> TokenCharT (internalPos p) }
  String			{ \p s -> TokenStringT (internalPos p) }
  Skip				{ \p s -> TokenSkip (internalPos p) }
  Close			  	{ \p s -> TokenClose (internalPos p) }
  Wait			  	{ \p s -> TokenWait (internalPos p) }
-- Keywords
  (rec|μ)                       { \p s -> TokenRec (internalPos p) }
  let                           { \p s -> TokenLet (internalPos p) }
  in                            { \p s -> TokenIn (internalPos p) }
  data                          { \p s -> TokenData (internalPos p) }
  type                          { \p s -> TokenType (internalPos p) }
  otherwise			{ \p s -> TokenOtherwise (internalPos p) }
  if				{ \p s -> TokenIf (internalPos p) }
  then				{ \p s -> TokenThen (internalPos p) }
  else				{ \p s -> TokenElse (internalPos p) }
--  new				{ \p s -> TokenNew (internalPos p) }
  select		        { \p s -> TokenSelect (internalPos p) }
  match				{ \p s -> TokenMatch (internalPos p) }
  with				{ \p s -> TokenWith (internalPos p) }
  case				{ \p s -> TokenCase (internalPos p) }
  of				{ \p s -> TokenOf (internalPos p) }
  (forall|∀)                    { \p s -> TokenForall (internalPos p) }
  dualof			{ \p s -> TokenDualof (internalPos p) }
-- Values
  \(\)				{ \p s -> TokenUnit (internalPos p) }
  (0+|[1-9]$digit*)    	{ \p s -> TokenInt (internalPos p) (read s) }
  ([\-\+]?@numspc@decimal"."@decimal@exponent?|@numspc@decimal@exponent) { \p s -> TokenFloat (internalPos p) (read $ filter (/= '_') s)}
  ([\-\+]?[0-9]+)i             { \p s -> TokenInteger (internalPos p) (read $ init s) }
  @char				{ \p s -> TokenChar (internalPos p) (read s) }
  @stringLiteral		{ \p s -> TokenString (internalPos p) (read s) }
-- Identifiers
  @lowerId                      { \p s -> TokenLowerId (internalPos p) s }
  @upperId                      { \p s -> TokenUpperId (internalPos p) s }

{

data Token =
    TokenNL Span
  | TokenIntT Span
  | TokenFloatT Span
  | TokenIntegerT Span
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
  | TokenClose Span
  | TokenWait Span
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
  | TokenPlusF Span
  | TokenPlusI Span
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
  | TokenFloat Span Double
  | TokenInteger Span Integer
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
  | TokenMinusDot Span
  | TokenMinusI Span
  | TokenTimes Span
  | TokenTimesDot Span
  | TokenTimesI Span
  | TokenRaise Span
  | TokenRaiseTimes Span
  | TokenRaiseI Span
  | TokenWild Span
  | TokenLT Span
  | TokenGT Span
  | TokenCmp Span String
  | TokenConjunction Span
  | TokenDisjunction Span
  | TokenAppend Span
  | TokenAppendString Span
  | TokenDiv Span
  | TokenDivDot Span
  | TokenDivI Span
  | TokenDollar Span
  | TokenModule Span
  | TokenWhere Span
  | TokenImport Span
  | TokenMutual Span
  | TokenAnd Span

instance Show Token where
  show (TokenNL _) = "\\n"
  show (TokenIntT _) = "Int"
  show (TokenFloatT _) = "Float"
  show (TokenIntegerT _) = "Integer"
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
  show (TokenClose _) = "Close"
  show (TokenWait _) = "Wait"
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
  show (TokenPlusF _) = "+."
  show (TokenPlusI _) = "+i"
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
  show (TokenFloat _ i) = show i 
  show (TokenInteger _ i) = show i 
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
  show (TokenMinusDot _) = "-."
  show (TokenMinusI _) = "-i"
  show (TokenTimes _) = "*"
  show (TokenTimesDot _) = "*."
  show (TokenTimesI _) = "*i"
  show (TokenRaise _) = "^"
  show (TokenRaiseTimes _) = "**"
  show (TokenRaiseI _) = "^i"
  show (TokenWild _) = "_"
  show (TokenLT _) = "<"
  show (TokenGT _) = ">"
  show (TokenCmp _ s) = show s
  show (TokenConjunction _) = "&&"
  show (TokenDisjunction _) = "||"
  show (TokenAppend _) = "++"
  show (TokenAppendString _) = "^^"
  show (TokenDiv _) = "/"
  show (TokenDivDot _) = "/."
  show (TokenDivI _) = "/i"
  show (TokenDollar _) = "$"
  show (TokenModule _) = "module"
  show (TokenWhere _)  = "where"
  show (TokenImport _)  = "import"
  show (TokenMutual _) = "mutual"
  show (TokenAnd _) = "and"

-- Trim newlines
scanTokens :: String -> FilePath -> Either ErrorType [Token] 
scanTokens input filename =
    case go (alexStartPos,'\n',[],input) of
      Right x -> Right $ trim x
      left -> left
  where
    go inp@(pos,_,_,input) =
      case alexScan inp 0 of
        AlexEOF -> Right []
        AlexError _ ->
          let p = internalPos pos in
          Left $ LexicalError (Span filename (startPos p) (endPos p)) (show $ head input)
        AlexSkip  inp' _     -> go inp'
        AlexToken inp' len act ->
          case go inp' of
            Right x -> Right $ act pos (take len input) : x
            tok -> tok

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

-- AlexPn offset lineNum colNum
getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL _ : ts) = trim' ts
    trim' ts = ts

-- POSITIONS

internalPos :: AlexPosn -> Span
internalPos (AlexPn _ l c) = let p = (l, c) in Span "<default>" p p

-- TODO: proper spans?, proper filename
instance Located Token where
  getSpan (TokenNL p) = p
  getSpan (TokenIntT p) = p
  getSpan (TokenFloatT p) = p
  getSpan (TokenIntegerT p) = p
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
  getSpan (TokenClose p) = p
  getSpan (TokenWait p) = p
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
  getSpan (TokenPlusF p) = p
  getSpan (TokenPlusI p) = p
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
  getSpan (TokenFloat p _) = p
  getSpan (TokenInteger p _) = p
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
  getSpan (TokenMinusDot p) = p
  getSpan (TokenMinusI p) = p
  getSpan (TokenTimes p) = p
  getSpan (TokenTimesDot p) = p
  getSpan (TokenTimesI p) = p
  getSpan (TokenRaise p) = p
  getSpan (TokenRaiseTimes p) = p
  getSpan (TokenRaiseI p) = p
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
  getSpan (TokenAppendString p) = p
  getSpan (TokenDiv p) = p
  getSpan (TokenDivDot p) = p
  getSpan (TokenDivI p) = p
  getSpan (TokenDollar p) = p
  getSpan (TokenModule p) = p
  getSpan (TokenWhere p) = p
  getSpan (TokenImport p) = p
  getSpan (TokenMutual p) = p 
  getSpan (TokenAnd p) = p 
--  pos t = error $ show t


getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenCmp _ x) = x
getText tok = internalError "Parse.Lexer.getText" tok

}
