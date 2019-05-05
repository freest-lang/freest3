{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
) where

import Syntax.Base
import Syntax.Show
}

%wrapper "posn"

$lowerU = [\927-\982] 
$lowerA = [a-z]
$lower = [$lowerA$lowerU]

$upperU = [\913-\937] -- TODO: ranges are wrong 
$upperA = [A-Z]
$upper = [$upperA$upperU]

$letter = [$lower$upper]
$symbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

$digit = 0-9
@char = \' ([\\.] | . ) \'
@lineComment  = \n*"--".* 
@blockComment = "{-" (\\.|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"

$eol=[\n]
  
tokens :-  
  $white*$eol+                  { \p s -> TokenNL (internalPos p) }
--  $eol+                         { \p s -> TokenNL (internalPos p) }
  $white+                       ;
  @lineComment                  ;
  @blockComment                 ;
  "->"				{ \p s -> TokenUnArrow (internalPos p) }
  "-o"				{ \p s -> TokenLinArrow (internalPos p) }
  "\"				{ \p s -> TokenLambda (internalPos p) }
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
  "&&"  		        { \p s -> TokenOp (internalPos p) "(&&)" }
  "||"  		        { \p s -> TokenOp (internalPos p) "(||)" }
-- Kinds
  SU                            { \p s -> TokenSU (internalPos p) }
  SL                            { \p s -> TokenSL (internalPos p) }
  TU                            { \p s -> TokenTU (internalPos p) }
  TL                            { \p s -> TokenTL (internalPos p) }
-- Types
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
  send				{ \p s -> TokenSend (internalPos p) }
  receive			{ \p s -> TokenReceive (internalPos p) }
  select			{ \p s -> TokenSelect (internalPos p) }
  match				{ \p s -> TokenMatch (internalPos p) }
  with				{ \p s -> TokenWith (internalPos p) }
  fork				{ \p s -> TokenFork (internalPos p) }
  case				{ \p s -> TokenCase (internalPos p) }
  of				{ \p s -> TokenOf (internalPos p) }
  forall			{ \p s -> TokenForall (internalPos p) }
  dualof			{ \p s -> TokenDualof (internalPos p) }
-- Values
  \(\)				{ \p s -> TokenUnit (internalPos p) }  
  (0|[1-9]$digit*)      	{ \p s -> TokenInteger (internalPos p) (read s) }
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
  | TokenSend Pos
  | TokenReceive Pos
  | TokenSelect Pos
  | TokenMatch Pos
  | TokenWith Pos
  | TokenFork Pos
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

instance Show Token where
  show (TokenNL p) = show p ++ ": NL"  
  show (TokenIntT p) = show p ++ ": Int"  
  show (TokenCharT p) = show p ++ ": Char"  
  show (TokenBoolT p) = show p ++ ": Bool"  
  show (TokenUnit p) = show p ++ ": ()"  
  show (TokenUnArrow p) = show p ++ ": ->"  
  show (TokenLinArrow p) = show p ++ ": -o"  
  show (TokenLambda p) = show p ++ ": \\"  
  show (TokenLParen p) = show p ++ ": ("
  show (TokenRParen p) = show p ++ ": )"  
  show (TokenLBracket p) = show p ++ ": ["  
  show (TokenRBracket p) = show p ++ ": ]"  
  show (TokenComma p) = show p ++ ": ,"  
  show (TokenSkip p) = show p ++ ": Skip" 
  show (TokenColon p) = show p ++ ": :"  
  show (TokenUpperId p c) = show p ++ ": " ++ c
  show (TokenSemi p) = show p ++ ": ;"  
  show (TokenMOut p) = show p ++ ": !"  
  show (TokenMIn p) = show p ++ ": ?"  
  show (TokenLBrace p) = show p ++ ": {"
  show (TokenRBrace p) = show p ++ ": }"
  show (TokenAmpersand p) = show p ++ ": &"
  show (TokenPlus p) = show p ++ ": +"
  show (TokenRec p) = show p ++ ": rec"
  show (TokenDot p) = show p ++ ": ."
  show (TokenLowerId p s) = show p ++ ": " ++ s
  show (TokenSU p) = show p ++ ": SU" 
  show (TokenSL p) = show p ++ ": SL"   
  show (TokenTU p) = show p ++ ": TU"   
  show (TokenTL p) = show p ++ ": TL"  
  show (TokenInteger p i) = show p ++ ": " ++ show i
  show (TokenChar p c) = show p ++ ": " ++ show c
  show (TokenBool p b) = show p ++ ": " ++ show b
  show (TokenLet p) = show p ++ ": let"
  show (TokenIn p) = show p ++ ": in"
  show (TokenEq p) = show p ++ ": ="
  show (TokenData p) = show p ++ ": data"  
  show (TokenType p) = show p ++ ": type"  
  show (TokenPipe p) = show p ++ ": |"  
  show (TokenIf p) = show p ++ ": if"  
  show (TokenThen p) = show p ++ ": then"  
  show (TokenElse p) = show p ++ ": else"  
  show (TokenNew p) = show p ++ ": new"  
  show (TokenSend p) = show p ++ ": send"  
  show (TokenReceive p) = show p ++ ": receive"  
  show (TokenSelect p) = show p ++ ": select"  
  show (TokenFork p) = show p ++ ": fork"  
  show (TokenMatch p) = show p ++ ": match"  
  show (TokenCase p) = show p ++ ": case"  
  show (TokenForall p) = show p ++ ": forall"  
  show (TokenMinus p) = show p ++ ": -"  
  show (TokenTimes p) = show p ++ ": *"  
  show (TokenLT p) = show p ++ ": <"
  show (TokenGT p) = show p ++ ": >"
  show (TokenWild p) = show p ++ ": _"  
  show (TokenOp p s) = show p ++ ": " ++ show s
  show (TokenOf p) = show p ++ ": of"  
  show (TokenDualof p) = show p ++ ": dualof"  
  show (TokenFArrow p) = show p ++ ": =>"

-- Trim newlines
scanTokens = alexScanTokens >>= (return . trim)

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
  position (TokenSend p) = p
  position (TokenReceive p) = p
  position (TokenSelect p) = p
  position (TokenFork p) = p
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
--  position t = error $ show t

getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenOp _ x) = x

}
