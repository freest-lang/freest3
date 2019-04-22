import           Syntax.Types
import           Syntax.Base
import Equivalence.Equivalence
import qualified Data.Map.Strict as Map

t1 = Skip defaultPos
t2 = Skip defaultPos

main = do
  print $ equivalent Map.empty Map.empty t1 t2
