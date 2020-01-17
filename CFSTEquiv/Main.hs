import System.Exit
import Equivalence.Equivalence
import Bisimulation.Bisimulation
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
  putStrLn "Insert two types separated by a new line character (Ctrl+d to terminate):"
  contents <- getContents
  case parseSchemes "CFSTEquiv" contents of
    Right err -> die err
    Left (t1, t2) -> do
      kindedType t1
      kindedType t2
      equivTypes t1 t2
  
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
