main : Int
main = sendFun 5

sendFun : Int -> Int
sendFun =
  let (w, r) = new @!(Int -> Int);End () in
  fork @() (\_:()1-> send (\x:Int -> x) w |> close);
  receiveAndClose @(Int->Int) r
