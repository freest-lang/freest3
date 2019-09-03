{
module Equivalence.GrammarParser (
  parseGrammar
, parseGrammars  
) where

import Equivalence.GrammarLexer
import Equivalence.Grammar
import Syntax.TypeVariables
import Syntax.Base
import Syntax.Types
import Prelude hiding (Word)
import qualified Data.Map.Strict as Map
}

%name grammar Grammar
%name grammars Grammars
%error { parseError }
%tokentype { Token }

%token
  Symbol {TokenString _}
  '->'   {TokenArrow}
  nl     {TokenNL}
  '('    {TokenLParen}
  ')'    {TokenRParen}
  '{'    {TokenLBracket}
  '}'    {TokenRBracket}
  ';'    {TokenSemi}
  ','    {TokenComma}
  Int    {TokenInt}
  Bool   {TokenBool}
  Char   {TokenChar}
  '+'    {TokenPlus}
  '&'    {TokenAmpersand}
  '!'    {TokenExclamation}
  '?'    {TokenQuestion}

%%

Grammars :: { [Grammar] }
  : '{' emptyNL Grammar emptyNL '}' emptyNL Grammars  { $3 : $7 }
  | '{' emptyNL Grammar emptyNL '}'               { [$3] }

emptyNL
  : NL {}
  |    {}
  
NL :: { () }
  : nl NL {}
  | nl    {}
  
Grammar :: { Grammar }
  : '(' Symbol ',' Symbol ')' NL Productions
         { Grammar [[mkVar defaultPos (show $2)], [mkVar defaultPos (show $4)]] $7 }
  
Productions :: { Productions } 
  : Production NL            { $1 }
  | Production NL Productions   { insertProd $3 $1 }
  
Production :: { Productions }
  : Symbol '->' Transition { Map.singleton (mkVar defaultPos (show $1)) $3 }

Transition :: { Transitions } -- more transitions??
  : Label Word    { Map.singleton $1 $2 }

Word :: { Word }
  : Symbol Word   { (mkVar defaultPos (show $1)) : $2 }
  | {- empty -}    { [] }

Label :: { Label }
  : PolarityMessage BasicType { MessageLabel $1 $2 } 
  | PolarityChoice Symbol     { ChoiceLabel $1 (mkVar defaultPos (show $2)) }
  | Symbol                    { VarLabel (mkVar defaultPos (show $1)) }

PolarityMessage :: { Polarity }
  : '!'  { Out }
  | '?'  { In }

PolarityChoice :: { Polarity }
  : '+'  { Out }
  | '&'  { In }

BasicType :: { BasicType }
  : Int    { IntType }
  | Bool   { BoolType }
  | Char   { CharType }
  | '('')' { UnitType }


{
parseGrammar :: String -> Grammar
parseGrammar = grammar . scanTokens

parseGrammars :: String -> [Grammar]
parseGrammars = grammars . scanTokens
  
parseError :: [Token] -> a
parseError ts = error $ "Parse error" ++ "\n" ++ show ts

insertProd :: Productions -> Productions -> Productions
insertProd ps = Map.foldlWithKey (\acc t ts -> insert t ts acc) ps
  where
    insert :: TypeVar -> Transitions -> Productions -> Productions
    insert t ts ps =
      case ps Map.!? t of
        Just tss -> Map.insert t (Map.union ts tss) ps
        otherwise -> Map.insert t ts ps

}
