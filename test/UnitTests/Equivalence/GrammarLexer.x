{
module Equivalence.GrammarLexer
(scanTokens,
 Token(..)) where
}

%wrapper "basic"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$symbol = [\#]
$string = [$upper$lower$digit$symbol]

$eol=[\n]

tokens :-
  $white*$eol+   { \s -> TokenNL }
  $white+        ;
  -- Basic Types
  Int            { \s -> TokenInt }
  Char		 { \s -> TokenChar }
  Bool		 { \s -> TokenBool }
  -- Symbols
  "+"            { \s -> TokenPlus }
  "&"            { \s -> TokenAmpersand }
  "!"            { \s -> TokenExclamation }
  "?"            { \s -> TokenQuestion }
  "->"           { \s -> TokenArrow }
  "("            { \s -> TokenLParen }
  ","            { \s -> TokenComma } 
  ")"            { \s -> TokenRParen }
  ";"            { \s -> TokenSemi }
  "{"            { \s -> TokenLBracket }
  "}"            { \s -> TokenRBracket }
  -- Strings
  $string+       { \s -> TokenString s }

{
data Token =
    TokenNL
  | TokenArrow
  | TokenString String
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenInt
  | TokenBool
  | TokenChar
  | TokenPlus
  | TokenAmpersand
  | TokenExclamation
  | TokenQuestion
  | TokenLBracket
  | TokenRBracket
  | TokenSemi
  
--  deriving Show

instance Show Token where
  show (TokenNL) = "\n"
  show (TokenArrow) = " -> "
  show (TokenString s) = s
  show (TokenLParen) = "("
  show (TokenRParen) = ")"
  show (TokenComma) = ","
  show (TokenInt) = "Int"
  show (TokenBool) = "Bool"
  show (TokenChar) = "Char"
  show (TokenPlus) = "+"
  show (TokenAmpersand) = "&"
  show (TokenExclamation) = "!"
  show (TokenQuestion) = "?"
  show (TokenSemi) = ";"
  show (TokenLBracket) = "{"
  show (TokenRBracket) = "}"

  
scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trim' . reverse . trim'
  where 
    trim' :: [Token] -> [Token]
    trim' [] = []
    trim' (TokenNL : ts) = trim' ts        
    trim' ts = ts

}
