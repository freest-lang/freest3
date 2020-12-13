{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where

import qualified Data.Map.Strict as Map
import           Parse.ParseUtils
import           Syntax.Base
import           Utils.Error
import           Utils.ErrorMessage
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
@blockComment = "{-" (\.*|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"

-- # λ  -- forall not in range ([λ ∀])
$greekId = [λ ∀ Λ μ]

@lowerId = ($lower # $greekId) $alphaNumeric*
@upperId = ($upper # $greekId) $alphaNumeric*

tokens :-  
  $white*$eol+                  { \p s -> TokenNL (internalPos p) }
  $white+                       ;  
  $eol*"--".*                   ;
  @blockComment                 ;
  "->"				{ \p s -> TokenUnArrow (internalPos p) }
  "-o"				{ \p s -> TokenLinArrow (internalPos p) }
  ("\"|λ)                       { \p s -> TokenLambda (internalPos p) }
  ("\\"|Λ)                      { \p s -> TokenUpperLambda (internalPos p) }
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
  ">"  	          		{ \p s -> TokenCmp (internalPos p) "(>)" }
  "<"  	          		{ \p s -> TokenCmp (internalPos p) "(<)" }
  ">="  		        { \p s -> TokenCmp (internalPos p) "(>=)" }
  "<="  		        { \p s -> TokenCmp (internalPos p) "(<=)" }
  "=="  		        { \p s -> TokenCmp (internalPos p) "(==)" }
  "/="  		        { \p s -> TokenCmp (internalPos p) "(/=)" }
  "&&"  		        { \p s -> TokenConjunction (internalPos p) }
  "||"  		        { \p s -> TokenDisjunction (internalPos p) }
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
  Int				{ \p s -> TokenIntT (internalPos p) }
  Char				{ \p s -> TokenCharT (internalPos p) }
  Bool				{ \p s -> TokenBoolT (internalPos p) }
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
  select			{ \p s -> TokenSelect (internalPos p) }
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
-- Identifiers
  "(+)" | "(-)" | "(*)"         { \p s -> TokenLowerId (internalPos p) s }  -- TODO: add remaining operators
  @lowerId                       { \p s -> TokenLowerId (internalPos p) s }
  @upperId                      { \p s -> TokenUpperId (internalPos p) s }

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
  | TokenCmp Pos String
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
  show (TokenLambda p) = "λ"  
  show (TokenUpperLambda p) = "Λ"  
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
  show (TokenMU p) = "MU"  
  show (TokenML p) = "ML"  
  show (TokenInt p i) = show i
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
  show (TokenCmp p s) = show s
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
  pos (TokenNL p) = p 
  pos (TokenIntT p) = p 
  pos (TokenCharT p) = p 
  pos (TokenBoolT p) = p 
  pos (TokenUnit p) = p 
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
--  pos t = error $ show t

getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenCmp _ x) = x

}
