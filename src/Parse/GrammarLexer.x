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
$message = [\!\?]
$symbol = [\+\&$message]
$string = [$upper$lower$digit$symbol]
$eol=[\n]

@lineComment  = \n*"--".* 
@blockComment = "{-" (\\.|[^\{\-]|\n|\-\-|[^$symbol].*)* "-}"
@lowerId = ($symbol)?$lower$string*
@upperId = ($symbol)?$upper$string*
@basicType = $message("Int"|"Bool"|"Char")

tokens :-
  $white*$eol+   { \_ -> TokenNL }
  $white+        ;
  @lineComment   ;
  @blockComment  ;
  "->"           { \_ -> TokenArrow }
  "("            { \_ -> TokenLParen }
  ")"            { \_ -> TokenRParen }
  ","            { \_ -> TokenComma }
  @basicType     { \s -> TokenLowerId s }
  @lowerId       { \s -> TokenLowerId s }
  @upperId       { \s -> TokenUpperId s }

{
data Token =
    TokenArrow
  | TokenNL
  | TokenLowerId String
  | TokenUpperId String
  | TokenLParen
  | TokenRParen
  | TokenComma deriving Show

  
scanTokens = alexScanTokens >>= (return . trim)

getText :: Token -> String
getText (TokenLowerId x) = x
getText (TokenUpperId x) = x

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where 
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL : ts) = trim' ts        
    trim' ts = ts


}
