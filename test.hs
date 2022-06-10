
import qualified Data.Map as Map
import Data.Function((&))

type M = Map.Map Int String

m = Map.empty
  & Map.insert 1 "1"
  & Map.insert 2 "2"
  & Map.insert 3 "3"

main = mapM (putStrLn) m