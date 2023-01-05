main : Int
main = sendClosure 3 5

sendClosure : Int -> (Int -> Int)
sendClosure x =
  let (w, r) = new @!(Int -> Int);End  () in
  fork (\_:()1-> send (\y:Int -> y + x) w |> close);
  receiveAndClose @(Int -> Int) r
