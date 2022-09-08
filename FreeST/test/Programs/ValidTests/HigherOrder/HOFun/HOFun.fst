main : Int
main = sendFun 5

sendFun : Int -> Int
sendFun =
  let (w, r) = new !(Int -> Int);End in
  fork (send (\x:Int -> x) w & close);
  let (f, r) = receive r in close r; f
