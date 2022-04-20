main : Int
main = sendClosure 3 5

sendClosure : Int -> (Int -> Int)
sendClosure x =
  let (w, r) = new !(Int -> Int) in
  fork $ send (\y:Int -> y + x) w;
  let (f, _) = receive r in f
