main : Int
main = sendFun 5

sendFun : Int -> Int
sendFun =
  let (w, r) = new !(Int -> Int) in
  fork (\_:() 1-> send (\x:Int -> x) w);
  let (f, _) = receive r in f
