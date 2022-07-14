
import Data.List

is1 :: Int -> Bool
is1 = (==) 1

match :: [Int] -> [([Int],Int)] -> ([Int],[([Int],Int)])
match [] x = ([],x)
match vs x =
  -- destruct
  let es   = map snd x in
  let pss1 = map fst x in
  let pss2 = transpose pss1 in
  let z1   = zip pss2 vs in
  let z2   = sortOn (not.is1.head.fst) z1 in
  -- reconstruct
  let vs'  = map snd z2 in
  let pss3 = map fst z2 in
  let pss4 = transpose pss3 in
  let x'   = zip pss4 es in
  (vs',x')
  -- pss2

main :: IO ()
main = putStrLn 
     $ show 
    --  $ transpose [[1,2,3]
    --              ,[1,2,3]]
    --  $ match [3,1,2] 
    --          [([3 ,1 ,2 ],0)
    --          ,([32,12,22],1)
    --          ,([33,13,23],2)]
     $ match [] 
             [([],0)
             ,([],1)
             ,([],2)]