module Main.EquivalenceMain where

import System.Exit
import Equivalence.Equivalence
import Equivalence.Bisimulation
import Parse.Parser
import Syntax.Types
import Syntax.Schemes
import Syntax.Kinds
import Utils.FreestState
import Control.Monad.State
import Validation.Kinding
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  contents <- getContents
  let (t1, t2) = parseSchemes contents
 --  t2 <- validateInput t1
  kindedType t1
  kindedType t2
  equivTypes t1 t2
  

-- validateInput :: String -> IO ()
-- validateInput t
--   | length t == 0 = die $ "Provide a valid input"
--   | otherwise     = kindedType (read t :: TypeScheme)

kindedType :: TypeScheme -> IO ()
kindedType t
  | kinded t = return ()
  | otherwise   = die $ "Type " ++ show t ++ " is not well-formed"

kinded :: TypeScheme -> Bool
kinded (TypeScheme _ xs t) = null (errors s)
  where (_, s) = runState (synthetise (fromTypeVarBinds xs) t) (initialState "Kind synthesis")
          
equivTypes :: TypeScheme -> TypeScheme -> IO ()
equivTypes t1 t2
  | equivalent Map.empty Map.empty t1 t2 =
      putStrLn "The two given types are equivalent"
  | otherwise                            =
      putStrLn "The two given types are *not* equivalent"
