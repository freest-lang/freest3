{-# LANGUAGE FlexibleContexts #-}
module Parse.Lexer
(
  reservedOp
, parens
, identifier
, reserved
, comma
, symbol
, whiteSpace
, lexeme
, dot
, colon
, braces
, squares
, commaSep1
, natural
, integer
, apostrophe
, constructor
, lowerIdentifier
, commaSep
, rec
, forall
, skip
) where

import           Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Token
import           Text.ParserCombinators.Parsec(lookAhead, upper, lower, between, string)

-- TOKENS
lexer :: Token.TokenParser u
lexer  = Token.makeTokenParser
        (haskellDef
        {
        Token.reservedOpNames = [";", "!", "?", "->", "-o", "+", "&", "=>",
                                "=", "+", "-", "*", "/", "mod", "rem", "&&",
                                "||", "not", "|", "->", "==", ">", "<"],
          
        Token.reservedNames = ["Int","Bool","Char", "Skip", "()",
                               "rec", "forall", "data", "TU", "TL", "SU", "SL",
                               "send", "receive", "()", "new", "in", "let",
                               "fork", "match", "with", "select", "case",
                               "of", "True", "False", "if", "then",
                               "else", "type", "data"]
        })

reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
comma      = Token.comma lexer
symbol     = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
lexeme     = Token.lexeme lexer
dot = Token.dot lexer
colon = Token.colon lexer
braces = Token.braces lexer
squares = Token.squares lexer
commaSep1 = Token.commaSep1 lexer
natural = Token.natural lexer
integer = Token.integer lexer
apostrophe p = between (string "'") (string "'") p
constructor = lookAhead upper >> identifier
lowerIdentifier = lookAhead lower >> identifier
commaSep = Token.commaSep lexer

rec    = reserved "rec"
forall = reserved "forall"
skip   = reserved "Skip"


