{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where

import qualified Data.Map.Strict as Map
import           Parse.ParseUtils
import           Syntax.Base
import           Utils.Errors
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

@lowerId = ($lower # [λ ∀ Λ]) $alphaNumeric*
@upperId = ($upper # [λ ∀ Λ]) $alphaNumeric*

@stringLiteral = \"(\\.|[^\"]|\n)*\"

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
-- Basic types
  Int				{ \p s -> TokenIntT (internalPos p) }
  Char				{ \p s -> TokenCharT (internalPos p) }
  Bool				{ \p s -> TokenBoolT (internalPos p) }
  String			{ \p s -> TokenStringT (internalPos p) }
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
  @stringLiteral		{ \p s -> TokenString (internalPos p) (read s) }
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
  | TokenWild Pos
  | TokenLT Pos
  | TokenGT Pos
  | TokenCmp Pos String
  | TokenConjunction Pos
  | TokenDisjunction Pos
  | TokenDiv Pos
  | TokenDollar Pos

instance Show Token where
  show (TokenNL _) = "\n"  
  show (TokenIntT _) = "Int"  
  show (TokenCharT _) = "Char"  
  show (TokenBoolT _) = "Bool"  
  show (TokenStringT _) = "String"  
  show (TokenUnit _) = "()"  
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
  show (TokenInt _ i) = show i
  show (TokenChar _ c) = show c
  show (TokenString _ s) = s
  show (TokenBool _ b) = show b
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
  position (TokenStringT p) = p 
  position (TokenUnit p) = p 
  position (TokenUnArrow p) = p 
  position (TokenLinArrow p) = p 
  position (TokenLambda p) = p 
  position (TokenUpperLambda p) = p 
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
  position (TokenInt p _) = p
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
  position (TokenCmp p _) = p
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
getText (TokenCmp _ x) = x

}
