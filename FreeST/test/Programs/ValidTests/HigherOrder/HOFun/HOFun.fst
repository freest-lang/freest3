main : Int
main = sendFun 5

sendFun : Int -> Int
sendFun =
  let (w, r) = new @(!(Int -> Int);Close) () in
  fork @() (\_:()1-> send (\x:Int -> x) w |> close);
  receiveAndWait @(Int->Int) r
