{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where

import qualified Data.Map.Strict as Map
import           Parse.ParseUtils
import           Syntax.Base
import           Syntax.Show
import           Utils.Errors
import           Utils.ErrorMessage
}

%wrapper "posn"

$greek = [\880-\1023] # λ  -- forall not in range ([λ ∀])

$upperA  = [A-Z]
$upper = [$upperA$greek]

-- $lowerU  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$lowerA = [a-z]
$lower = [$lowerA $greek \_]

$letter = [$lower$upper]

$unidigit  = \x03
$ascdigit = 0-9
$digit = [$ascdigit $unidigit]

-- $ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol = [$ascsymbol] -- $unisymbol]
  
$eol=[\n]

@char = \'(\\n|[\\.]|.) \'
@blockComment = "{-" (\.*|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"
-- @blockComment = "{-" (\\.|[^\{\-]|\n|\||\")* "-}"
-- @blockComment = "{-"($eol|.*)*"-}"
  
tokens :-  
  $white*$eol+                  { \p s -> TokenNL (internalPos p) }
  $white+                       ;  
  $eol*"--".*                   ;
  @blockComment                 ;
  "->"				{ \p s -> TokenUnArrow (internalPos p) }
  "-o"				{ \p s -> TokenLinArrow (internalPos p) }
  ("\"|λ)                       { \p s -> TokenLambda (internalPos p) }
  "=>"				{ \p s -> TokenFArrow (internalPos p) }
  "("				{ \p s -> TokenLParen (internalPos p) }
  ")"				{ \p s -> TokenRParen (internalPos p) }
  "["				{ \p s -> TokenLBracket (internalPos p) }
  "]"			        { \p s -> TokenRBracket (internalPos p) }
  "{"				{ \p s -> TokenLBrace (internalPos p) }
  "}"			        { \p s -> TokenRBrace (internalPos p) }
  ","				{ \p s -> TokenComma (internalPos p) }
  ":"                           { \p s -> TokenColon (internalPos p) }   
  ";"	       	      	  	{ \p s -> TokenSemi (internalPos p) }
  "!"				{ \p s -> TokenMOut (internalPos p) }
  "?"				{ \p s -> TokenMIn (internalPos p) }
  "&"				{ \p s -> TokenAmpersand (internalPos p) }
  "."                           { \p s -> TokenDot (internalPos p) }  
  "="                           { \p s -> TokenEq (internalPos p) }
  "|"                           { \p s -> TokenPipe (internalPos p) }
-- Operators
  "+"				{ \p s -> TokenPlus (internalPos p) }   
  "-"				{ \p s -> TokenMinus (internalPos p) }
  "*"				{ \p s -> TokenTimes (internalPos p) }
  "_"				{ \p s -> TokenWild (internalPos p) }
  ">"  	          		{ \p s -> TokenOp (internalPos p) "(>)" }
  "<"  	          		{ \p s -> TokenOp (internalPos p) "(<)" }
  ">="  		        { \p s -> TokenOp (internalPos p) "(>=)" }
  "<="  		        { \p s -> TokenOp (internalPos p) "(<=)" }
  "=="  		        { \p s -> TokenOp (internalPos p) "(==)" }
  "/="  		        { \p s -> TokenOp (internalPos p) "(/=)" }
  "&&"  		        { \p s -> TokenConjunction (internalPos p) }
  "||"  		        { \p s -> TokenDisjunction (internalPos p) }
  "/"  		                { \p s -> TokenDiv (internalPos p) }
  "$"  		                { \p s -> TokenDollar (internalPos p) }
-- Kinds
  SU                            { \p s -> TokenSU (internalPos p) }
  SL                            { \p s -> TokenSL (internalPos p) }
  TU                            { \p s -> TokenTU (internalPos p) }
  TL                            { \p s -> TokenTL (internalPos p) }
-- Basic types
  Int				{ \p s -> TokenIntT (internalPos p) }
  Char				{ \p s -> TokenCharT (internalPos p) }
  Bool				{ \p s -> TokenBoolT (internalPos p) }
  Skip				{ \p s -> TokenSkip (internalPos p) }
-- Keywords
  rec                           { \p s -> TokenRec (internalPos p) }   
  let                           { \p s -> TokenLet (internalPos p) }
  in                            { \p s -> TokenIn (internalPos p) }
  data                          { \p s -> TokenData (internalPos p) }
  type                          { \p s -> TokenType (internalPos p) }
  if				{ \p s -> TokenIf (internalPos p) }
  then				{ \p s -> TokenThen (internalPos p) }
  else				{ \p s -> TokenElse (internalPos p) }
  new				{ \p s -> TokenNew (internalPos p) }
--  send				{ \p s -> TokenSend (internalPos p) }
--  receive			{ \p s -> TokenReceive (internalPos p) }
  select			{ \p s -> TokenSelect (internalPos p) }
  match				{ \p s -> TokenMatch (internalPos p) }
  with				{ \p s -> TokenWith (internalPos p) }
--  fork				{ \p s -> TokenFork (internalPos p) }
  case				{ \p s -> TokenCase (internalPos p) }
  of				{ \p s -> TokenOf (internalPos p) }
  (forall|∀)                    { \p s -> TokenForall (internalPos p) }
  dualof			{ \p s -> TokenDualof (internalPos p) }
-- Values
  \(\)				{ \p s -> TokenUnit (internalPos p) }  
  (0+|[1-9]$digit*)      	{ \p s -> TokenInteger (internalPos p) (read s) }
  (True|False) 	      	 	{ \p s -> TokenBool (internalPos p) (read s) }
  @char				{ \p s -> TokenChar (internalPos p) (read s) }
-- Identifiers
  "(+)" | "(-)" | "(*)"         { \p s -> TokenLowerId (internalPos p) s }  -- TODO: add remaining operators
  $lower [$letter$digit\_\']*   { \p s -> TokenLowerId (internalPos p) s }
  $upper [$letter$digit\_\']*	{ \p s -> TokenUpperId (internalPos p) s }

{

data Token =
    TokenNL Pos 
  | TokenIntT Pos 
  | TokenCharT Pos 
  | TokenBoolT Pos 
  | TokenUnit Pos 
  | TokenUnArrow Pos 
  | TokenLinArrow Pos 
  | TokenLambda Pos 
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
  | TokenInteger Pos Int
  | TokenChar Pos Char
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
  | TokenWild Pos
  | TokenLT Pos
  | TokenGT Pos
  | TokenOp Pos String
  | TokenConjunction Pos
  | TokenDisjunction Pos
  | TokenDiv Pos
  | TokenDollar Pos


instance Show Token where
  show (TokenNL p) = "\n"  
  show (TokenIntT p) = "Int"  
  show (TokenCharT p) = "Char"  
  show (TokenBoolT p) = "Bool"  
  show (TokenUnit p) = "()"  
  show (TokenUnArrow p) = "->"  
  show (TokenLinArrow p) = "-o"  
  show (TokenLambda p) = "\\"  
  show (TokenLParen p) = "("
  show (TokenRParen p) = ")"  
  show (TokenLBracket p) = "["  
  show (TokenRBracket p) = "]"  
  show (TokenComma p) = ","  
  show (TokenSkip p) = "Skip" 
  show (TokenColon p) = ":"  
  show (TokenUpperId p c) = "" ++ c
  show (TokenSemi p) = ";"  
  show (TokenMOut p) = "!"  
  show (TokenMIn p) = "?"  
  show (TokenLBrace p) = "{"
  show (TokenRBrace p) = "}"
  show (TokenAmpersand p) = "&"
  show (TokenPlus p) = "+"
  show (TokenRec p) = "rec"
  show (TokenDot p) = "."
  show (TokenLowerId p s) = "" ++ s
  show (TokenSU p) = "SU" 
  show (TokenSL p) = "SL"   
  show (TokenTU p) = "TU"   
  show (TokenTL p) = "TL"  
  show (TokenInteger p i) = show i
  show (TokenChar p c) = show c
  show (TokenBool p b) = show b
  show (TokenLet p) = "let"
  show (TokenIn p) = "in"
  show (TokenEq p) = "="
  show (TokenData p) = "data"  
  show (TokenType p) = "type"  
  show (TokenPipe p) = "|"  
  show (TokenIf p) = "if"  
  show (TokenThen p) = "then"  
  show (TokenElse p) = "else"  
  show (TokenNew p) = "new"  
--  show (TokenSend p) = "send"  
--  show (TokenReceive p) = "receive"  
  show (TokenSelect p) = "select"  
--  show (TokenFork p) = "fork"  
  show (TokenMatch p) = "match"  
  show (TokenCase p) = "case"  
  show (TokenForall p) = "forall"  
  show (TokenMinus p) = "-"  
  show (TokenTimes p) = "*"  
  show (TokenLT p) = "<"
  show (TokenGT p) = ">"
  show (TokenWild p) = "_"  
  show (TokenOp p s) = show s
  show (TokenOf p) = "of"  
  show (TokenDualof p) = "dualof"  
  show (TokenFArrow p) = "=>"
  show (TokenConjunction p) = "&&"
  show (TokenDisjunction p) = "||"
  show (TokenDiv p) = "/"
  show (TokenDollar p) = "$"

-- Trim newlines
scanTokens :: String -> String -> Either [Token] String
scanTokens str file =
    case go (alexStartPos,'\n',[],str) of
      Left x -> Left $ trim x
      x -> x
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> Left []
        AlexError _ ->
          Right $ formatErrorMessages Map.empty (internalPos pos) file
                  [Error "Lexical error on input",
                   Error $ "\ESC[91m" ++ show (head str) ++ "\ESC[0m"]
        AlexSkip  inp' len     -> go inp'
        AlexToken inp' len act -> 
          case go inp' of
            Left x -> Left $ act pos (take len str) : x
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
  position (TokenNL p) = p 
  position (TokenIntT p) = p 
  position (TokenCharT p) = p 
  position (TokenBoolT p) = p 
  position (TokenUnit p) = p 
  position (TokenUnArrow p) = p 
  position (TokenLinArrow p) = p 
  position (TokenLambda p) = p 
  position (TokenLParen p) = p
  position (TokenRParen p) = p 
  position (TokenLBracket p) = p 
  position (TokenRBracket p) = p 
  position (TokenComma p) = p 
  position (TokenSkip p) = p 
  position (TokenColon p) = p 
  position (TokenUpperId p _) = p
  position (TokenSemi p) = p 
  position (TokenMOut p) = p 
  position (TokenMIn p) = p 
  position (TokenLBrace p) = p
  position (TokenRBrace p) = p 
  position (TokenAmpersand p) = p 
  position (TokenPlus p) = p 
  position (TokenRec p) = p 
  position (TokenDot p) = p 
  position (TokenLowerId p _) = p 
  position (TokenSU p) = p 
  position (TokenSL p) = p 
  position (TokenTU p) = p 
  position (TokenTL p) = p
  position (TokenInteger p _) = p
  position (TokenChar p _) = p
  position (TokenBool p _) = p
  position (TokenLet p) = p 
  position (TokenIn p) = p
  position (TokenEq p) = p
  position (TokenData p) = p
  position (TokenType p) = p
  position (TokenPipe p) = p
  position (TokenNew p) = p
--  position (TokenSend p) = p
--  position (TokenReceive p) = p
  position (TokenSelect p) = p
--  position (TokenFork p) = p
  position (TokenMatch p) = p
  position (TokenCase p) = p
  position (TokenForall p) = p
  position (TokenMinus p) = p
  position (TokenTimes p) = p
  position (TokenLT p) = p
  position (TokenGT p) = p
  position (TokenWild p) = p
  position (TokenOp p _) = p
  position (TokenIf p) = p
  position (TokenThen p) = p
  position (TokenElse p) = p
  position (TokenWith p) = p
  position (TokenOf p) = p
  position (TokenDualof p) = p
  position (TokenFArrow p) = p
  position (TokenConjunction p) = p
  position (TokenDisjunction p) = p
  position (TokenDiv p) = p
  position (TokenDollar p) = p
--  position t = error $ show t

getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenOp _ x) = x

}
