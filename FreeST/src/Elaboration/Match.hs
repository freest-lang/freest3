module Elaboration.Match
  ( matchFun
    match
  )
where

import           Syntax.Base
import           Syntax.Expression
import qualified Data.Map.Strict   as Map

import           Util.FreestState

var :: Variable
var = Varibale undefined id
  where id = "#u"++(show 0) -- TODO change 0

vars :: Int -> [Variable]
vars n = replicate n var

-- just to remember the format
-- cases
-- type FieldMap  = Map.Map Variable ([Variable], Exp)
-- type FieldMapP = Map.Map Variable ([Pattern], Exp)
-- functions
-- type ParseEnv  = Map.Map Variable ([Variable], Exp)
-- type ParseEnvP = Map.Map Variable [([Pattern], Exp)]

--                fun args       patterns, exp     otherwise
data Match = M ( [Variable] , [([Pattern], Exp)] , Match    )
           | ERROR

matchFun :: ParseEnvP -> ParseEnv
matchFun pep = Map.map (match vs) pep
  where vs = vars n
-- TODO
-- transformar uma funcção num case
-- traduzir o case
-- adicionar o case na funcao

match :: [Variable] -> [([Pattern], Exp)] -> ([Variable], Exp)
match vs ps = undefined
-- TODO
-- criar len [Pattern] variaveis, porque sao os argumentos
-- transformar [([Pattern],Exp)] into Match
-- identificar a rule correta
-- chamar a rule sobre o estado
-- recursivo até chegar à rule empty?

-- > ainda tenho de descobrir como fazer os cases

ruleEmpty :: Match -> Match
ruleEmpty x = x

ruleVar :: Match -> Match
ruleVar x = x

ruleCon :: Match -> Match
ruleCon x = x

ruleMix :: Match -> Match
ruleMix x = x