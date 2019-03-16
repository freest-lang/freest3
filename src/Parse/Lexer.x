{
module Parse.Lexer
( Token(..)
, scanTokens
, AlexPosn(..)
, getPos)
where

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
@lineComment  = $eol*"--".* 
@blockComment = "{-" (\\.|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"
  
tokens :-    
  $white*$eol                           {\p s -> TokenNL p}
  $eol+                                 {\p s -> TokenNL p}
  $white+                                ;
  @lineComment                           ;
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
  \{					{\p s -> TokenLBrace p}
  \}			                {\p s -> TokenRBrace p}
  [\,]					{\p s -> TokenComma p}
  Skip					{\p s -> TokenSkip p}
  [\:]  			       	{\p s -> TokenColon p}  
  [\;]	       	      	  		{\p s -> TokenSemi p}
  [\!]					{\p s -> TokenMOut p}
  [\?]					{\p s -> TokenMIn p}
  [\&]					{\p s -> TokenAmpersand p}
  [\+]					{\p s -> TokenPlus p}  
  [\-]					{\p s -> TokenMinus p}
  [\*]					{\p s -> TokenTimes p}
  [\_]					{\p s -> TokenWild p}
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
  type                                  {\p s -> TokenType p}
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
  dualof				{\p s -> TokenDualof p}
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
  | TokenLBrace AlexPosn 
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
-- Operators
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenWild AlexPosn
  | TokenLT AlexPosn
  | TokenGT AlexPosn
  | TokenOp AlexPosn String
--  deriving Show

instance Show Token where
  show (TokenNL p) = show p ++ ": \\n"
  show (TokenIntT p) = show p ++ ": Int"  
  show (TokenCharT p) = show p ++ ": Char"  
  show (TokenBoolT p) = show p ++ ": Bool"  
  show (TokenUnitT p) = show p ++ ": ()"  
  show (TokenUnArrow p) = show p ++ ": ->"  
  show (TokenLinArrow p) = show p ++ ": -o"  
  show (TokenLParen p) = show p ++ ": ("
  show (TokenRParen p) = show p ++ ": )"  
  show (TokenLBracket p) = show p ++ ": ["  
  show (TokenRBracket p) = show p ++ ": ]"  
  show (TokenComma p) = show p ++ ": ,"  
  show (TokenSkip p) = show p ++ ": Skip" 
  show (TokenColon p) = show p ++ ": :"  
  show (TokenCons p c) = show p ++ ": " ++ c
  show (TokenSemi p) = show p ++ ": ;"  
  show (TokenMOut p) = show p ++ ": !"  
  show (TokenMIn p) = show p ++ ": ?"  
  show (TokenLBrace p) = show p ++ ": {"
  show (TokenRBrace p) = show p ++ ": }"
  show (TokenAmpersand p) = show p ++ ": &"
  show (TokenPlus p) = show p ++ ": +"
  show (TokenRec p) = show p ++ ": rec"
  show (TokenDot p) = show p ++ ": ."
  show (TokenVar p s) = show p ++ ": " ++ s
  show (TokenSU p) = show p ++ ": SU" 
  show (TokenSL p) = show p ++ ": SL"   
  show (TokenTU p) = show p ++ ": TU"   
  show (TokenTL p) = show p ++ ": TL"  
-- Expressions
  -- Basic expressions
  -- Unit already defined
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
-- Operators
  show (TokenMinus p) = show p ++ ": -"  
  show (TokenTimes p) = show p ++ ": *"  
  show (TokenWild p) = show p ++ ": _"  
  show (TokenOp p s) = show p ++ ": " ++ show s  
  -- show (TokenLT p) = show p ++ ": <"  
  -- show (TokenGT p) = show p ++ ": >"  

-- TODO: instance show token for errors

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where 
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL _ : ts) = trim' ts        
    trim' ts = ts


-- TODO: create class Token
-- TODO: -> change to instance position
getPos :: Token -> AlexPosn
getPos (TokenIntT p) = p 
getPos (TokenCharT p) = p 
getPos (TokenBoolT p) = p 
getPos (TokenUnitT p) = p 
getPos (TokenUnArrow p) = p 
getPos (TokenLinArrow p) = p 
getPos (TokenLParen p) = p
getPos (TokenRParen p) = p 
getPos (TokenLBracket p) = p 
getPos (TokenRBracket p) = p 
getPos (TokenComma p) = p 
getPos (TokenSkip p) = p 
getPos (TokenColon p) = p 
getPos (TokenCons p _) = p
getPos (TokenSemi p) = p 
getPos (TokenMOut p) = p 
getPos (TokenMIn p) = p 
getPos (TokenLBrace p) = p
getPos (TokenRBrace p) = p 
getPos (TokenAmpersand p) = p 
getPos (TokenPlus p) = p 
getPos (TokenRec p) = p 
getPos (TokenDot p) = p 
getPos (TokenVar p _) = p 
getPos (TokenSU p) = p 
getPos (TokenSL p) = p 
getPos (TokenTU p) = p 
getPos (TokenTL p) = p
getPos (TokenInteger p _) = p
getPos (TokenBool p _) = p
getPos (TokenChar p _) = p
getPos (TokenLet p) = p 
getPos (TokenIn p) = p
getPos (TokenEq p) = p
getPos (TokenData p) = p
getPos (TokenType p) = p
getPos (TokenPipe p) = p
getPos (TokenNL p) = p
getPos (TokenNew p) = p
getPos (TokenSend p) = p
getPos (TokenReceive p) = p
getPos (TokenSelect p) = p
getPos (TokenFork p) = p
getPos (TokenMatch p) = p
getPos (TokenCase p) = p
getPos (TokenForall p) = p
getPos (TokenDualof p) = p
getPos (TokenMinus p) = p
getPos (TokenTimes p) = p
getPos (TokenLT p) = p
getPos (TokenGT p) = p
getPos (TokenOp p _) = p
getPos t = error $ show t

-- pos (AlexPn _ l c) = (l,c) 

}
