main : Int
main = sendClosure 3 5

sendClosure : Int -> (Int -> Int)
sendClosure x =
  let (w, r) = new @(!(Int -> Int);Close)  () in
  fork (\_:()1-> send (\y:Int -> y + x) w |> close);
  receiveAndWait @(Int -> Int) r
