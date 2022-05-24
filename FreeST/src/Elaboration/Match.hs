module Elaboration.Match
  ( matchFun
  )
where

import           Syntax.Base
import           Syntax.Expression
import qualified Data.Map.Strict   as Map

import           Util.FreestState

-- type FieldMap  = Map.Map Variable ([Variable], Exp)
-- type FieldMapP = Map.Map Variable ([Pattern], Exp)

--                 fun args       patterns, exp     otherwise
type Matching = ( [Variable] , [([Pattern], Exp)] , Exp       )

-- TODO
matchFun :: ParseEnvP -> ParseEnv
matchFun pep = Map.map (match) pep

match :: [([Pattern], Exp)] -> ([Variable], Exp)
match ps = (undefined, undefined)
-- TODO
-- criar len [Pattern] variaveis, porque sao os argumentos
-- transformar [([Pattern],Exp)] into Matching
-- identificar a rule correta
-- chamar a rule sobre o estado
-- recursivo até chegar à rule empty?

-- > ainda tenho de descobrir como fazer os cases

ruleEmpty :: Matching -> Matching
ruleEmpty x = x

ruleVar :: Matching -> Matching
ruleVar x = x

ruleCon :: Matching -> Matching
ruleCon x = x

ruleMix :: Matching -> Matching
ruleMix x = x