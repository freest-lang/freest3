{
module Parse.Lexer (Token(..),scanTokens, getPos, pos) where

-- import GHC.Generics
}

%wrapper "posn"

$eol   = [\n]

$lowerU = [\927-\982] 
$lowerA = [a-z]
$lower = [$lowerA $lowerU]

$upperU = [\913-\937] -- TODO: ranges are wrong 
$upperA = [A-Z]
$upper = [$upperA $upperU]

$var = [$lower$upper\_\=]
$symbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

$digit = 0-9
-- @charL = \' [$var $digit \_] \
@char = \' ([\\.] | . ) \'

@blockComment = "{-" (\\.|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"
  
tokens :-  
  $eol*"--".*                               ;
  $white*$eol                           {\p s -> TokenNL p}
  $eol+                                 {\p s -> TokenNL p}
  $white+                                ;
  @blockComment                          ;
  Int					{\p s -> TokenIntT p}
  Char					{\p s -> TokenCharT p}
  Bool					{\p s -> TokenBoolT p}
  \(\)					{\p s -> TokenUnitT p}
  "->"					{\p s -> TokenUnArrow p}
  "=>"					{\p s -> TokenFArrow p}
  "-o"					{\p s -> TokenLinArrow p}
  \(					{\p s -> TokenLParen p}
  \)					{\p s -> TokenRParen p}
  \[					{\p s -> TokenLBracket p}
  \]			                {\p s -> TokenRBracket p}
  \+\{					{\p s -> TokenLIChoice p}
  \&\{					{\p s -> TokenLEChoice p}
  \}			                {\p s -> TokenRBrace p}
  [\,]					{\p s -> TokenComma p}
  Skip					{\p s -> TokenSkip p}
  [\:]  			       	{\p s -> TokenColon p}  
  [\;]	       	      	  		{\p s -> TokenSemi p}
  [\!]					{\p s -> TokenMOut p}
  [\?]					{\p s -> TokenMIn p}
--  [\&]					{\p s -> TokenAmpersand p}
  [\+]					{\p s -> TokenPlus p}  
  [\-]					{\p s -> TokenMinus p}
  [\*]					{\p s -> TokenTimes p}
  -- [\<]					{\p s -> TokenLT p}
  -- [\>]					{\p s -> TokenGT p}
  rec                                   {\p s -> TokenRec p}  
  [\.]                                  {\p s -> TokenDot p} 
  SU                                    {\p s -> TokenSU p}
  SL                                    {\p s -> TokenSL p}
  TU                                    {\p s -> TokenTU p}
  TL                                    {\p s -> TokenTL p}
  $digit+      	      	 		{\p s -> TokenInteger p $ read s}
  (True|False) 	      	 		{\p s -> TokenBool p $ read s}
  let                                   {\p s -> TokenLet p}
  \=                                    {\p s -> TokenEq p}
  in                                    {\p s -> TokenIn p}
  data                                  {\p s -> TokenData p}
  \|                                    {\p s -> TokenPipe p}
  if					{\p s -> TokenIf p}
  then					{\p s -> TokenThen p}
  else					{\p s -> TokenElse p}
  new					{\p s -> TokenNew p}
  send					{\p s -> TokenSend p}
  receive				{\p s -> TokenReceive p}
  select				{\p s -> TokenSelect p}
  match					{\p s -> TokenMatch p}
  with					{\p s -> TokenWith p}
  fork					{\p s -> TokenFork p}
  case					{\p s -> TokenCase p}
  of					{\p s -> TokenOf p}
  forall				{\p s -> TokenForall p}
  -- Operators
  [\>\<]  	          		{\p s -> TokenOp p ("(" ++ s ++ ")")}
  \>\=  			        {\p s -> TokenOp p ("(>=)")}
  \<\=  			        {\p s -> TokenOp p ("(<=)")}
  \=\=  			        {\p s -> TokenOp p ("(==)")}
  \&\&  			        {\p s -> TokenOp p ("(&&)")}
  \|\|  			        {\p s -> TokenOp p ("(||)")}
  -- Variables
  $lower [$var $digit \_ \']*           {\p s -> TokenVar p s}
  @char					{\p s -> TokenChar p $ read s}
  $upper [$var $digit \_ \']* 		{\p s -> TokenCons p s}

  
{
-- False      	      	 		{\p s -> TokenBool p False}
data Token =
    TokenNL AlexPosn 
  | TokenIntT AlexPosn 
  | TokenCharT AlexPosn 
  | TokenBoolT AlexPosn 
  | TokenUnitT AlexPosn 
  | TokenUnArrow AlexPosn 
  | TokenLinArrow AlexPosn 
  | TokenLParen AlexPosn 
  | TokenRParen AlexPosn 
  | TokenLBracket AlexPosn 
  | TokenRBracket AlexPosn 
  | TokenComma AlexPosn 
  | TokenSkip AlexPosn 
  | TokenColon AlexPosn 
  | TokenCons AlexPosn String
  | TokenSemi AlexPosn 
  | TokenMOut AlexPosn 
  | TokenMIn AlexPosn 
--  | TokenLBrace AlexPosn 
  | TokenLIChoice AlexPosn 
  | TokenLEChoice AlexPosn 
  | TokenRBrace AlexPosn 
  | TokenAmpersand AlexPosn 
  | TokenPlus AlexPosn 
  | TokenRec AlexPosn 
  | TokenDot AlexPosn 
  | TokenVar AlexPosn String  
  | TokenSU AlexPosn 
  | TokenSL AlexPosn 
  | TokenTU AlexPosn 
  | TokenTL AlexPosn
-- Expressions
  -- Basic expressions
  -- Unit already defined
  | TokenInteger AlexPosn Int
  | TokenChar AlexPosn Char
  | TokenBool AlexPosn Bool
  | TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenEq AlexPosn
  | TokenData AlexPosn
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
  | TokenFArrow AlexPosn
-- Operators
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenLT AlexPosn
  | TokenGT AlexPosn
  | TokenOp AlexPosn String
--  deriving Show

instance Show Token where
  show (TokenNL p) = show (pos p) ++ ": \\n"
  show (TokenIntT p) = show (pos p) ++ ": Int"  
  show (TokenCharT p) = show (pos p) ++ ": Char"  
  show (TokenBoolT p) = show (pos p) ++ ": Bool"  
  show (TokenUnitT p) = show (pos p) ++ ": ()"  
  show (TokenUnArrow p) = show (pos p) ++ ": ->"  
  show (TokenLinArrow p) = show (pos p) ++ ": -o"  
  show (TokenLParen p) = show (pos p) ++ ": ("
  show (TokenRParen p) = show (pos p) ++ ": )"  
  show (TokenLBracket p) = show (pos p) ++ ": ["  
  show (TokenRBracket p) = show (pos p) ++ ": ]"  
  show (TokenComma p) = show (pos p) ++ ": ,"  
  show (TokenSkip p) = show (pos p) ++ ": Skip" 
  show (TokenColon p) = show (pos p) ++ ": :"  
  show (TokenCons p c) = show (pos p) ++ ": " ++ c
  show (TokenSemi p) = show (pos p) ++ ": ;"  
  show (TokenMOut p) = show (pos p) ++ ": !"  
  show (TokenMIn p) = show (pos p) ++ ": ?"  
--  show (TokenLBrace p) = show (pos p) ++ ": {"
  show (TokenLIChoice p) = show (pos p) ++ ": +{"
  show (TokenLEChoice p) = show (pos p) ++ ": &{"
  show (TokenRBrace p) = show (pos p) ++ ": }"
--  show (TokenAmpersand p) = show (pos p) ++ ": &"
  show (TokenPlus p) = show (pos p) ++ ": +"
  show (TokenRec p) = show (pos p) ++ ": rec"
  show (TokenDot p) = show (pos p) ++ ": ."
  show (TokenVar p s) = show (pos p) ++ ": " ++ s
  show (TokenSU p) = show (pos p) ++ ": SU" 
  show (TokenSL p) = show (pos p) ++ ": SL"   
  show (TokenTU p) = show (pos p) ++ ": TU"   
  show (TokenTL p) = show (pos p) ++ ": TL"  
-- Expressions
  -- Basic expressions
  -- Unit already defined
  show (TokenInteger p i) = show (pos p) ++ ": " ++ show i
  show (TokenChar p c) = show (pos p) ++ ": " ++ show c
  show (TokenBool p b) = show (pos p) ++ ": " ++ show b
  show (TokenLet p) = show (pos p) ++ ": let"
  show (TokenIn p) = show (pos p) ++ ": in"
  show (TokenEq p) = show (pos p) ++ ": ="
  show (TokenData p) = show (pos p) ++ ": data"  
  show (TokenPipe p) = show (pos p) ++ ": |"  
  show (TokenIf p) = show (pos p) ++ ": if"  
  show (TokenThen p) = show (pos p) ++ ": then"  
  show (TokenElse p) = show (pos p) ++ ": else"  
  show (TokenNew p) = show (pos p) ++ ": new"  
  show (TokenSend p) = show (pos p) ++ ": send"  
  show (TokenReceive p) = show (pos p) ++ ": receive"  
  show (TokenSelect p) = show (pos p) ++ ": select"  
  show (TokenMatch p) = show (pos p) ++ ": match"  
  show (TokenWith p) = show (pos p) ++ ": with"  
  show (TokenFork p) = show (pos p) ++ ": fork"  
  show (TokenCase p) = show (pos p) ++ ": case"  
  show (TokenOf p) = show (pos p) ++ ": of"  
  show (TokenForall p) = show (pos p) ++ ": forall"  
  show (TokenFArrow p) = show (pos p) ++ ": =>"
-- Operators
  show (TokenMinus p) = show (pos p) ++ ": -"  
  show (TokenTimes p) = show (pos p) ++ ": *"  
  show (TokenOp p s) = show (pos p) ++ ": " ++ show s  
  -- show (TokenLT p) = show (pos p) ++ ": <"  
  -- show (TokenGT p) = show (pos p) ++ ": >"  

-- TODO: instance show token for errors

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where 
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL _ : ts) = trim' ts        
    trim' ts = ts


-- TMP -> change to generic
getPos :: Token -> (Int, Int)
getPos (TokenIntT p) = pos p 
getPos (TokenCharT p) = pos p 
getPos (TokenBoolT p) = pos p 
getPos (TokenUnitT p) = pos p 
getPos (TokenUnArrow p) = pos p 
getPos (TokenLinArrow p) = pos p 
getPos (TokenLParen p) = pos p
getPos (TokenRParen p) = pos p 
getPos (TokenLBracket p) = pos p 
getPos (TokenRBracket p) = pos p 
getPos (TokenComma p) = pos p 
getPos (TokenSkip p) = pos p 
getPos (TokenColon p) = pos p 
getPos (TokenCons p _) = pos p
getPos (TokenSemi p) = pos p 
getPos (TokenMOut p) = pos p 
getPos (TokenMIn p) = pos p 
-- getPos (TokenLBrace p) = pos p
getPos (TokenLIChoice p) = pos p
getPos (TokenLEChoice p) = pos p
getPos (TokenRBrace p) = pos p 
getPos (TokenAmpersand p) = pos p 
getPos (TokenPlus p) = pos p 
getPos (TokenRec p) = pos p 
getPos (TokenDot p) = pos p 
getPos (TokenVar p _) = pos p 
getPos (TokenSU p) = pos p 
getPos (TokenSL p) = pos p 
getPos (TokenTU p) = pos p 
getPos (TokenTL p) = pos p
getPos (TokenInteger p _) = pos p
getPos (TokenBool p _) = pos p
getPos (TokenChar p _) = pos p
getPos (TokenLet p) = pos p 
getPos (TokenIn p) = pos p
getPos (TokenEq p) = pos p
getPos (TokenData p) = pos p
getPos (TokenPipe p) = pos p
-- Some missing
getPos (TokenNL p) = pos p
getPos (TokenNew p) = pos p
getPos (TokenSend p) = pos p
getPos (TokenReceive p) = pos p
getPos (TokenSelect p) = pos p
getPos (TokenFork p) = pos p
getPos (TokenMatch p) = pos p
getPos (TokenCase p) = pos p
getPos (TokenForall p) = pos p
getPos (TokenMinus p) = pos p
getPos (TokenTimes p) = pos p
getPos (TokenLT p) = pos p
getPos (TokenGT p) = pos p
getPos (TokenOp p _) = pos p

pos (AlexPn _ l c) = (l,c) 

}
