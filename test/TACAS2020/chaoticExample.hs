-- import           TACAS2020.Bisimulation0
-- import           TACAS2020.Bisimulation1
-- import           TACAS2020.Bisimulation2
-- import           TACAS2020.Bisimulation3
-- import           TACAS2020.Bisimulation4
-- import           TACAS2020.Bisimulation12
-- import           TACAS2020.Bisimulation123 -- Loops?
import           TACAS2020.Bisimulation1234
import           Parse.Parser
import qualified Data.Map.Strict as Map

main = do
  let t = read "((rec y0.(+{A: z, C: ?Int};(&{B: y0, C: z};y0)));Skip)"
      u = read "+{A: (z;&{B: ((rec y10.+{A: (z;&{B: (y10;y10), C: (z;y10)}), C: (?Int;&{B: (y10;y10), C: (z;y10)})});(rec y21.+{A: (z;&{B: (y21;y21), C: (z;y21)}), C: (?Int;&{B: (y21;y21), C: (z;y21)})})), C: (z;(rec y32.+{A: (z;&{B: (y32;y32), C: (z;y32)}), C: (?Int;&{B: (y32;y32), C: (z;y32)})}))}), C: (?Int;&{B: ((rec y43.+{A: (z;&{B: (y43;y43), C: (z;y43)}), C: (?Int;&{B: (y43;y43), C: (z;y43)})});(rec y54.+{A: (z;&{B: (y54;y54), C: (z;y54)}), C: (?Int;&{B: (y54;y54), C: (z;y54)})})), C: (z;(rec y65.+{A: (z;&{B: (y65;y65), C: (z;y65)}), C: (?Int;&{B: (y65;y65), C: (z;y65)})}))})}"
      -- Notice that these two types are renamed
  putStrLn $ "Is\n  " ++ show t ++ "\nbisimilar to\n  " ++ show u ++ "\n?"
  print $ bisimilar Map.empty u t
  return ()
