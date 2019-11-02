{
module Parse.GrammarParser (
   parseGrammar
) where

import Parse.GrammarLexer
import Equivalence.Grammar
import Syntax.TypeVariables
import Syntax.Base
import Syntax.Types
import Prelude hiding (Word)
import qualified Data.Map.Strict as Map
}

%name grammar Grammar
-- %name grammars Grammars
%error { parseError }
%tokentype { Token }

%token
  nl       { TokenNL }
  '->'     { TokenArrow }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  ','      { TokenComma }
  LOWER_ID { TokenLowerId _ }
  UPPER_ID { TokenUpperId _ }

%%

Grammar :: { Grammar }
  : '(' Word ',' Word ')' NL Productions { Grammar [$2, $4] $7 }

Productions :: { Productions }
  : Production                { $1 }
  | Production NL Productions {% insertProd $3 $1 }

Production :: { Productions }
  :                             { Map.empty }
  | NonTerminal '->' Transition { Map.singleton $1 $3 }
  
Transition : Terminal Word { Map.singleton $1 $2 }

Word :: { Word }
  :                  { [] }
  | NonTerminal Word { $1 : $2 }

Terminal :: { String }
  : LOWER_ID { getText $1 }
  
NonTerminal :: { TypeVar }
  : UPPER_ID { mkVar defaultPos (getText $1) }

NL :: { () }
  : nl NL {}
  | nl    {}

{
parseGrammar :: String -> Grammar
parseGrammar = grammar . scanTokens

parseError :: [Token] -> a
parseError ts = error $ "Parse error" ++ "\n" ++ show ts

insertProd :: Productions -> Productions -> Productions
insertProd ps = Map.foldlWithKey (\acc t ts -> insert t ts acc) ps
  where
    insert :: TypeVar -> Transitions -> Productions -> Productions
    insert t ts ps =
      case ps Map.!? t of
        Just tss -> 
          -- putStrLn $ "Duplicated production for X -> a. Ignoring the former; using the latter."
          Map.insert t (Map.union ts tss) ps
        otherwise -> Map.insert t ts ps

instance Read Grammar where
  readsPrec _ str = [(parseGrammar str, "")]

-- type Errors = [String]
-- type GrammarState = State Errors

-- initialState :: Errors
-- initialState = []

-- addError :: String -> GrammarState ()
-- addError s = modify (\state -> s : state)
  
}
