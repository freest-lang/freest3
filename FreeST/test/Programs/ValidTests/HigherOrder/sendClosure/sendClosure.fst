plus : Int -> Int -> Int
plus x y = x + y

main : Int
main =
  let n = 23 in
  let x = forkWith @?(Int -> Int);End @() (\y: !(Int -> Int);End 1-> send (plus n) y |> close) in
  (receiveAndClose @(Int -> Int) x) 27
