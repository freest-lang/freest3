-- import Bisimulation.Grammar
import Equivalence.Equivalence
import qualified Syntax.Type as T
import Syntax.Base
import qualified Data.Map as Map

main :: IO ()
main = do
  let t1 = T.Int defaultPos
  let t2 = T.Bool defaultPos
  print $ equivalent Map.empty t1 t1
