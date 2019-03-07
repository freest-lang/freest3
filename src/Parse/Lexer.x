{
module Parse.Lexer (Token(..),scanTokens, getPos, posAlexToPos) where

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
  | TokenLT AlexPosn
  | TokenGT AlexPosn
  | TokenOp AlexPosn String
--  deriving Show

instance Show Token where
  show (TokenNL p) = show (posAlexToPos p) ++ ": \\n"
  show (TokenIntT p) = show (posAlexToPos p) ++ ": Int"  
  show (TokenCharT p) = show (posAlexToPos p) ++ ": Char"  
  show (TokenBoolT p) = show (posAlexToPos p) ++ ": Bool"  
  show (TokenUnitT p) = show (posAlexToPos p) ++ ": ()"  
  show (TokenUnArrow p) = show (posAlexToPos p) ++ ": ->"  
  show (TokenLinArrow p) = show (posAlexToPos p) ++ ": -o"  
  show (TokenLParen p) = show (posAlexToPos p) ++ ": ("
  show (TokenRParen p) = show (posAlexToPos p) ++ ": )"  
  show (TokenLBracket p) = show (posAlexToPos p) ++ ": ["  
  show (TokenRBracket p) = show (posAlexToPos p) ++ ": ]"  
  show (TokenComma p) = show (posAlexToPos p) ++ ": ,"  
  show (TokenSkip p) = show (posAlexToPos p) ++ ": Skip" 
  show (TokenColon p) = show (posAlexToPos p) ++ ": :"  
  show (TokenCons p c) = show (posAlexToPos p) ++ ": " ++ c
  show (TokenSemi p) = show (posAlexToPos p) ++ ": ;"  
  show (TokenMOut p) = show (posAlexToPos p) ++ ": !"  
  show (TokenMIn p) = show (posAlexToPos p) ++ ": ?"  
  show (TokenLBrace p) = show (posAlexToPos p) ++ ": {"
  show (TokenRBrace p) = show (posAlexToPos p) ++ ": }"
  show (TokenAmpersand p) = show (posAlexToPos p) ++ ": &"
  show (TokenPlus p) = show (posAlexToPos p) ++ ": +"
  show (TokenRec p) = show (posAlexToPos p) ++ ": rec"
  show (TokenDot p) = show (posAlexToPos p) ++ ": ."
  show (TokenVar p s) = show (posAlexToPos p) ++ ": " ++ s
  show (TokenSU p) = show (posAlexToPos p) ++ ": SU" 
  show (TokenSL p) = show (posAlexToPos p) ++ ": SL"   
  show (TokenTU p) = show (posAlexToPos p) ++ ": TU"   
  show (TokenTL p) = show (posAlexToPos p) ++ ": TL"  
-- Expressions
  -- Basic expressions
  -- Unit already defined
  show (TokenInteger p i) = show (posAlexToPos p) ++ ": " ++ show i
  show (TokenChar p c) = show (posAlexToPos p) ++ ": " ++ show c
  show (TokenBool p b) = show (posAlexToPos p) ++ ": " ++ show b
  show (TokenLet p) = show (posAlexToPos p) ++ ": let"
  show (TokenIn p) = show (posAlexToPos p) ++ ": in"
  show (TokenEq p) = show (posAlexToPos p) ++ ": ="
  show (TokenData p) = show (posAlexToPos p) ++ ": data"  
  show (TokenType p) = show (posAlexToPos p) ++ ": type"  
  show (TokenPipe p) = show (posAlexToPos p) ++ ": |"  
  show (TokenIf p) = show (posAlexToPos p) ++ ": if"  
  show (TokenThen p) = show (posAlexToPos p) ++ ": then"  
  show (TokenElse p) = show (posAlexToPos p) ++ ": else"  
  show (TokenNew p) = show (posAlexToPos p) ++ ": new"  
  show (TokenSend p) = show (posAlexToPos p) ++ ": send"  
  show (TokenReceive p) = show (posAlexToPos p) ++ ": receive"  
  show (TokenSelect p) = show (posAlexToPos p) ++ ": select"  
  show (TokenMatch p) = show (posAlexToPos p) ++ ": match"  
  show (TokenWith p) = show (posAlexToPos p) ++ ": with"  
  show (TokenFork p) = show (posAlexToPos p) ++ ": fork"  
  show (TokenCase p) = show (posAlexToPos p) ++ ": case"  
  show (TokenOf p) = show (posAlexToPos p) ++ ": of"  
  show (TokenForall p) = show (posAlexToPos p) ++ ": forall"  
  show (TokenDualof p) = show (posAlexToPos p) ++ ": dualof"  
  show (TokenFArrow p) = show (posAlexToPos p) ++ ": =>"
-- Operators
  show (TokenMinus p) = show (posAlexToPos p) ++ ": -"  
  show (TokenTimes p) = show (posAlexToPos p) ++ ": *"  
  show (TokenOp p s) = show (posAlexToPos p) ++ ": " ++ show s  
  -- show (TokenLT p) = show (posAlexToPos p) ++ ": <"  
  -- show (TokenGT p) = show (posAlexToPos p) ++ ": >"  

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
getPos (TokenIntT p) = posAlexToPos p 
getPos (TokenCharT p) = posAlexToPos p 
getPos (TokenBoolT p) = posAlexToPos p 
getPos (TokenUnitT p) = posAlexToPos p 
getPos (TokenUnArrow p) = posAlexToPos p 
getPos (TokenLinArrow p) = posAlexToPos p 
getPos (TokenLParen p) = posAlexToPos p
getPos (TokenRParen p) = posAlexToPos p 
getPos (TokenLBracket p) = posAlexToPos p 
getPos (TokenRBracket p) = posAlexToPos p 
getPos (TokenComma p) = posAlexToPos p 
getPos (TokenSkip p) = posAlexToPos p 
getPos (TokenColon p) = posAlexToPos p 
getPos (TokenCons p _) = posAlexToPos p
getPos (TokenSemi p) = posAlexToPos p 
getPos (TokenMOut p) = posAlexToPos p 
getPos (TokenMIn p) = posAlexToPos p 
getPos (TokenLBrace p) = posAlexToPos p
getPos (TokenRBrace p) = posAlexToPos p 
getPos (TokenAmpersand p) = posAlexToPos p 
getPos (TokenPlus p) = posAlexToPos p 
getPos (TokenRec p) = posAlexToPos p 
getPos (TokenDot p) = posAlexToPos p 
getPos (TokenVar p _) = posAlexToPos p 
getPos (TokenSU p) = posAlexToPos p 
getPos (TokenSL p) = posAlexToPos p 
getPos (TokenTU p) = posAlexToPos p 
getPos (TokenTL p) = posAlexToPos p
getPos (TokenInteger p _) = posAlexToPos p
getPos (TokenBool p _) = posAlexToPos p
getPos (TokenChar p _) = posAlexToPos p
getPos (TokenLet p) = posAlexToPos p 
getPos (TokenIn p) = posAlexToPos p
getPos (TokenEq p) = posAlexToPos p
getPos (TokenData p) = posAlexToPos p
getPos (TokenType p) = posAlexToPos p
getPos (TokenPipe p) = posAlexToPos p
getPos (TokenNL p) = posAlexToPos p
getPos (TokenNew p) = posAlexToPos p
getPos (TokenSend p) = posAlexToPos p
getPos (TokenReceive p) = posAlexToPos p
getPos (TokenSelect p) = posAlexToPos p
getPos (TokenFork p) = posAlexToPos p
getPos (TokenMatch p) = posAlexToPos p
getPos (TokenCase p) = posAlexToPos p
getPos (TokenForall p) = posAlexToPos p
getPos (TokenDualof p) = posAlexToPos p
getPos (TokenMinus p) = posAlexToPos p
getPos (TokenTimes p) = posAlexToPos p
getPos (TokenLT p) = posAlexToPos p
getPos (TokenGT p) = posAlexToPos p
getPos (TokenOp p _) = posAlexToPos p
getPos t = error $ show t

posAlexToPos (AlexPn _ l c) = (l,c) 

}
