plus : Int -> Int -> Int
plus x y = x + y

main : Int
main =
  let n = 23 in
  let x = forkWith @(?(Int -> Int);EndW) @() (\y: !(Int -> Int);EndC 1-> send (plus n) y |> close) in
  (receiveAndWait @(Int -> Int) x) 27
