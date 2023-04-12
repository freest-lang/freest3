{
module GrammarParser (
   parseGrammar
) where

import GrammarLexer
import Bisimulation.Grammar
import Syntax.Base
import Syntax.Type
import Prelude hiding (Word)
import qualified Data.Map.Strict as Map
import System.Exit
import Data.List (intercalate)
}

%name grammar Grammar
%error { parseError }
%tokentype { Token }
%monad { IO }

%token
  '->'     { TokenArrow }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  ','      { TokenComma }
  LOWER_ID { TokenLowerId _ }
  UPPER_ID { TokenUpperId _ }
  SYM      { TokenSymbol _ }

%%
  
Grammar :: { Grammar }
  : '(' Word ',' Word ')' Productions { Grammar [$2, $4] $6 }

Productions :: { Productions }
  :  { Map.empty }
  | NonTerminal '->' Terminal Word Productions
     { insertProd $5 (Map.singleton $1 (Map.singleton $3 $4)) }

Word :: { Word }
  :                  { [] }
  | Word NonTerminal { $1 ++ [$2] }

Terminal :: { String }
  : LOWER_ID  { getText $1 }
  | SYM LOWER_ID    { getText $1 ++ getText $2 }
  | SYM UPPER_ID    { getText $1 ++ getText $2 }
    
NonTerminal :: { Variable }
  : UPPER_ID  { mkVar defaultPos (getText $1) }

{
parseGrammar :: String -> IO Grammar
parseGrammar = grammar . scanTokens

parseError :: [Token] -> IO a
parseError [] = putStrLn "Parse error, possibly a premature end of the input" >> exitFailure
parseError (t:_) = putStrLn ("Parse error on token: " ++ show t) >> exitFailure

insertProd :: Productions -> Productions -> Productions
insertProd = Map.foldlWithKey (\acc t ts -> insert t ts acc)
  where
    insert :: Variable -> Transitions -> Productions -> Productions
    insert t ts ps = do
      case ps Map.!? t of
        Just tss -> Map.insert t (Map.union ts tss) ps
        otherwise -> Map.insert t ts ps

-- insertProd :: Productions -> Productions -> IO Productions
-- insertProd ps = Map.foldlWithKey (\acc t ts -> insert t ts acc) (return ps)
--   where
--     insert :: TypeVar -> Transitions -> IO Productions -> IO Productions
--     insert t ts ps1 = do
--       ps <- ps1
--       case ps Map.!? t of
--         Just tss -> do 
--           putStrLn $ "Duplicated production for " ++ show tss ++
--                      ". Ignoring the former; using the latter."
--           return $ Map.insert t (Map.union ts tss) ps
--         otherwise -> return $ Map.insert t ts ps
  
}
