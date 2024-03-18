module PatternMatch.PatternMatch (patternMatch) where

import qualified Parse.Phase as PP
import           PatternMatch.Match
import           PatternMatch.Phase
import           Syntax.AST
import           Util.State

import qualified Data.Map.Strict as Map
import           Control.Monad.State hiding (void)

patternMatch :: PP.ParseS -> PatternS
patternMatch parseS =
  execState (patternMatching (getDefsS parseS)) (parseToPattern parseS)
  
-- | 1. Fix the multiplicity of the data constructor types
-- | 2. Checks if there are choices with the same name as constructors (no need for now)
-- | 3. Checks correct number of arguments
-- | 4. Checks correct channels' pattern matching
-- | 5. Adds missing Vars to malformed functions
-- | 6. Remove all patterns
patternMatching :: PP.Defs -> PatternState ()
patternMatching s = do
  checkChoices s
  checkNumArgs s
  checkChanVar s
  let s1 = addMissingVars s
  matchFuns s1 >>= setDefs

parseToPattern :: PP.ParseS -> PatternS
parseToPattern s = s {ast = (ast s){definitions = Map.empty}, extra = void}
