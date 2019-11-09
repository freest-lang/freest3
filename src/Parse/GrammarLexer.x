{
module Parse.GrammarLexer
(scanTokens,
 Token(..),
 getText) where
}

%wrapper "basic"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$eol=[\n]

-- except ( ) - ,
$symbol = [\`\~\@\#\$\%\^\&\*\_\+\=\{\[\}\]\:\;\'\"\<\>\.\/\?\!]
$string = [$upper$lower$digit$symbol]

@lowerId = $lower$string*|$symbol+$string*
@upperId = $upper$string*
@lineComment  = \n*"--".* 
@blockComment = "{-" (\\.|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"

tokens :-
  $white*$eol+   ; 
  $white+        ;
  @lineComment   ;
  @blockComment  ;
  "->"           { \_ -> TokenArrow }
  "("            { \_ -> TokenLParen }
  ")"            { \_ -> TokenRParen }
  ","            { \_ -> TokenComma }
  @lowerId       { \s -> TokenLowerId s }
  $symbol+       { \s -> TokenSymbol s }
  @upperId       { \s -> TokenUpperId s }

{
data Token =
    TokenArrow
  | TokenLowerId String
  | TokenUpperId String
  | TokenSymbol String
  | TokenLParen
  | TokenRParen
  | TokenComma
  
instance Show Token where
  show TokenArrow = "->"
  show (TokenLowerId x) = x
  show (TokenUpperId x) = x
  show (TokenSymbol x) = x
  show TokenLParen = "("
  show TokenRParen = ")"
  show TokenComma = "," 


scanTokens = alexScanTokens

getText :: Token -> String
getText (TokenLowerId x) = x
getText (TokenUpperId x) = x
getText (TokenSymbol x) = x
}
