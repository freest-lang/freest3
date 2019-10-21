import           QuickCheck.ArbitraryTypes
import           Test.QuickCheck
import           Syntax.Schemes
import           Syntax.Types
import           QuickCheck.TestValidTypes
import qualified Data.Map as Map
import qualified TACAS2020.Bisimulation0 as B0
import qualified TACAS2020.Bisimulation1 as B1
import qualified TACAS2020.Bisimulation2 as B2
import qualified TACAS2020.Bisimulation3 as B3
import qualified TACAS2020.Bisimulation4 as B4
import qualified TACAS2020.Bisimulation12 as B12
import qualified TACAS2020.Bisimulation123 as B123
import qualified TACAS2020.Bisimulation1234 as B1234

bisims = [ B0.bisimilar
         , B1.bisimilar
         , B2.bisimilar
         , B3.bisimilar
         , B4.bisimilar
         , B12.bisimilar
         , B123.bisimilar
         , B1234.bisimilar
         ]

tests :: BisimPair -> [Property]
tests ts = map (test ts) bisims

test :: BisimPair -> (TypeEnv -> Type -> Type -> Bool) -> Property
test (BisimPair t u) bisim = kinded t && kinded u ==> bisim Map.empty t u
