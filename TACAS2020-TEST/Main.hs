import qualified Bisimulation0 as B0
-- B1
import qualified Bisimulation2 as B2
import qualified Bisimulation3 as B3
import qualified Bisimulation4 as B4
 -- B12
 -- B13
 -- B14
import qualified Bisimulation23 as B23
import qualified Bisimulation23 as B24
import qualified Bisimulation23 as B34
-- B123
-- B124
-- B134
import qualified Bisimulation234 as B234
import qualified Bisimulation1234 as B1234

import qualified TypeToGrammar as TG
import qualified TypeToGrammar1 as TG1


-- 1 + binomial(4, 1) + binomial(4, 2) + binomial(4, 3) + 1

bisimCombs :: TypeEnv -> Type -> Type -> [(String, TypeEnv -> Type -> Type -> Bool)]
bisimCombs tenv t u =
  [ ("B0", Bisimulation0.bisimilar $ TG.convertToGrammar)
  , ("B1", Bisimulation0.bisimilar $ TG1.convertToGrammar)
  , ("B2", Bisimulation2.bisimilar $ TG.convertToGrammar)
  , ("B3", Bisimulation3.bisimilar $ TG.convertToGrammar)
  , ("B4", Bisimulation4.bisimilar $ TG.convertToGrammar)
  , ("B12", Bisimulation2.bisimilar $ TG1.convertToGrammar)
  , ("B13", Bisimulation3.bisimilar $ TG1.convertToGrammar)
  , ("B14", Bisimulation4.bisimilar $ TG1.convertToGrammar)
  , ("B23", Bisimulation23.bisimilar $ TG.convertToGrammar)
  , ("B24", Bisimulation23.bisimilar $ TG.convertToGrammar)
  , ("B34", Bisimulation34.bisimilar $ TG.convertToGrammar)  
  , ("B123", Bisimulation23.bisimilar $ TG1.convertToGrammar)
  , ("B124", Bisimulation24.bisimilar $ TG1.convertToGrammar)
  , ("B134", Bisimulation34.bisimilar $ TG1.convertToGrammar)
  , ("B234", Bisimulation234.bisimilar $ TG.convertToGrammar)
  , ("B1234", Bisimulation234.bisimilar $ TG1.convertToGrammar)
  ]

bisimilar :: TypeEnv -> Type -> Type -> Bool
bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]                       

-- bisimilar :: TypeEnv -> Type -> Type -> Bool
-- bisimilar tEnv t u = Bisimulation.bisimilar $ convertToGrammar tEnv [t, u]
