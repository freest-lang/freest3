main : Int
main = sendClosure 3 5

sendClosure : Int -> (Int -> Int)
sendClosure x =
  let (w, r) = new !(Int -> Int);End in
  fork $ send (\y:Int -> y + x) w;
  let (f, r) = receive r in close r; f
