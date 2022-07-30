plus : Int -> Int -> Int
plus x y = x + y

main : Int
main =
  let n = 23 in
  let x = forkWith @?(Int -> Int) (\y: !(Int -> Int) 1-> sink @Skip $ send (plus n) y) in
  (fst @Int -> Int @Skip (receive x)) 27
