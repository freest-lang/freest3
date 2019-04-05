{
module Parse.Lexer
( Token(..)
, scanTokens
, getText
, Pos
, Position(..)
, defaultPos -- Should not be needed
, showPos
) where
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
  $white*$eol+                  { \p s -> TokenNL p }
--  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  @lineComment                  ;
  @blockComment                 ;
  "->"				{ \p s -> TokenUnArrow p }
  "-o"				{ \p s -> TokenLinArrow p }
  "=>"				{ \p s -> TokenFArrow p }
  "("				{ \p s -> TokenLParen p }
  ")"				{ \p s -> TokenRParen p }
  "["				{ \p s -> TokenLBracket p }
  "]"			        { \p s -> TokenRBracket p }
  "{"				{ \p s -> TokenLBrace p }
  "}"			        { \p s -> TokenRBrace p }
  ","				{ \p s -> TokenComma p }
  ":"                           { \p s -> TokenColon p}   
  ";"	       	      	  	{ \p s -> TokenSemi p }
  "!"				{ \p s -> TokenMOut p }
  "?"				{ \p s -> TokenMIn p }
  "&"				{ \p s -> TokenAmpersand p }
  "."                           { \p s -> TokenDot p}  
  "="                           { \p s -> TokenEq p }
  "|"                           { \p s -> TokenPipe p }
-- Operators
  "+"				{ \p s -> TokenPlus p}   
  "-"				{ \p s -> TokenMinus p }
  "*"				{ \p s -> TokenTimes p }
  "_"				{ \p s -> TokenWild p }
  ">"  	          		{ \p s -> TokenOp p "(>)" }
  "<"  	          		{ \p s -> TokenOp p "(<)" }
  ">="  		        { \p s -> TokenOp p "(>=)" }
  "<="  		        { \p s -> TokenOp p "(<=)" }
  "=="  		        { \p s -> TokenOp p "(==)" }
  "&&"  		        { \p s -> TokenOp p "(&&)" }
  "||"  		        { \p s -> TokenOp p "(||)" }
-- Kinds
  SU                            { \p s -> TokenSU p }
  SL                            { \p s -> TokenSL p }
  TU                            { \p s -> TokenTU p }
  TL                            { \p s -> TokenTL p }
-- Types
  Int				{ \p s -> TokenIntT p }
  Char				{ \p s -> TokenCharT p }
  Bool				{ \p s -> TokenBoolT p }
  Skip				{ \p s -> TokenSkip p }
-- Keywords
  rec                           { \p s -> TokenRec p}   
  let                           { \p s -> TokenLet p }
  in                            { \p s -> TokenIn p }
  data                          { \p s -> TokenData p }
  type                          { \p s -> TokenType p }
  if				{ \p s -> TokenIf p }
  then				{ \p s -> TokenThen p }
  else				{ \p s -> TokenElse p }
  new				{ \p s -> TokenNew p }
  send				{ \p s -> TokenSend p }
  receive			{ \p s -> TokenReceive p }
  select			{ \p s -> TokenSelect p }
  match				{ \p s -> TokenMatch p }
  with				{ \p s -> TokenWith p }
  fork				{ \p s -> TokenFork p }
  case				{ \p s -> TokenCase p }
  of				{ \p s -> TokenOf p }
  forall			{ \p s -> TokenForall p }
  dualof			{ \p s -> TokenDualof p }
-- Values
  \(\)				{ \p s -> TokenUnit p }  
  (0|[1-9]$digit*)      	{ \p s -> TokenInteger p $ read s }
  (True|False) 	      	 	{ \p s -> TokenBool p $ read s }
  @char				{ \p s -> TokenChar p $ read s }
-- Identifiers
  "(+)" | "(-)" | "(*)"         { TokenLowerId }  -- TODO: add remaining operators
  $lower [$letter$digit\_\']*   { TokenLowerId }
  $upper [$letter$digit\_\']*	{ TokenUpperId }

{

data Token =
    TokenNL AlexPosn 
  | TokenIntT AlexPosn 
  | TokenCharT AlexPosn 
  | TokenBoolT AlexPosn 
  | TokenUnit AlexPosn 
  | TokenUnArrow AlexPosn 
  | TokenLinArrow AlexPosn 
  | TokenLParen AlexPosn 
  | TokenRParen AlexPosn 
  | TokenLBracket AlexPosn 
  | TokenRBracket AlexPosn 
  | TokenComma AlexPosn 
  | TokenSkip AlexPosn 
  | TokenColon AlexPosn 
  | TokenUpperId AlexPosn String
  | TokenSemi AlexPosn 
  | TokenMOut AlexPosn 
  | TokenMIn AlexPosn 
  | TokenLBrace AlexPosn 
  | TokenRBrace AlexPosn 
  | TokenAmpersand AlexPosn 
  | TokenPlus AlexPosn 
  | TokenRec AlexPosn 
  | TokenDot AlexPosn 
  | TokenLowerId AlexPosn String  
  | TokenSU AlexPosn 
  | TokenSL AlexPosn 
  | TokenTU AlexPosn 
  | TokenTL AlexPosn
  | TokenInteger AlexPosn Int
  | TokenChar AlexPosn Char
  | TokenBool AlexPosn Bool
  | TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenEq AlexPosn
  | TokenData AlexPosn
  | TokenType AlexPosn
  | TokenPipe AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenNew AlexPosn
  | TokenSend AlexPosn
  | TokenReceive AlexPosn
  | TokenSelect AlexPosn
  | TokenMatch AlexPosn
  | TokenWith AlexPosn
  | TokenFork AlexPosn
  | TokenCase AlexPosn
  | TokenOf AlexPosn
  | TokenForall AlexPosn
  | TokenDualof AlexPosn 
  | TokenFArrow AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenWild AlexPosn
  | TokenLT AlexPosn
  | TokenGT AlexPosn
  | TokenOp AlexPosn String

instance Show Token where
  show (TokenNL p) = show p ++ ": NL"  
  show (TokenIntT p) = show p ++ ": Int"  
  show (TokenCharT p) = show p ++ ": Char"  
  show (TokenBoolT p) = show p ++ ": Bool"  
  show (TokenUnit p) = show p ++ ": ()"  
  show (TokenUnArrow p) = show p ++ ": ->"  
  show (TokenLinArrow p) = show p ++ ": -o"  
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
  show (TokenMatch p) = show p ++ ": match"  
  show (TokenWith p) = show p ++ ": with"  
  show (TokenFork p) = show p ++ ": fork"  
  show (TokenCase p) = show p ++ ": case"  
  show (TokenOf p) = show p ++ ": of"  
  show (TokenForall p) = show p ++ ": forall"  
  show (TokenDualof p) = show p ++ ": dualof"  
  show (TokenFArrow p) = show p ++ ": =>"
  show (TokenMinus p) = show p ++ ": -"  
  show (TokenTimes p) = show p ++ ": *"  
  show (TokenWild p) = show p ++ ": _"  
  show (TokenOp p s) = show p ++ ": " ++ show s  


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

type Pos = AlexPosn

class Position t where
  position :: t -> Pos

instance Ord AlexPosn where
  (AlexPn x _ _) `compare` (AlexPn y _ _ ) = x `compare` y

instance Position Token where
  position (TokenNL p) = p 
  position (TokenIntT p) = p 
  position (TokenCharT p) = p 
  position (TokenBoolT p) = p 
  position (TokenUnit p) = p 
  position (TokenUnArrow p) = p 
  position (TokenLinArrow p) = p 
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
  position (TokenBool p _) = p
  position (TokenChar p _) = p
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
  position (TokenDualof p) = p
  position (TokenMinus p) = p
  position (TokenTimes p) = p
  position (TokenLT p) = p
  position (TokenGT p) = p
  position (TokenOp p _) = p
  position (TokenIf p) = p
  position (TokenThen p) = p
  position (TokenElse p) = p
  position t = error $ show t

defaultPos :: Pos
defaultPos = AlexPn 0 0 0

showPos :: Pos -> String
showPos (AlexPn _ line column) = show line ++ ":" ++ show column

getText :: Token -> String
getText (TokenUpperId _ x) = x
getText (TokenLowerId _ x) = x
getText (TokenOp _ x) = x

}
