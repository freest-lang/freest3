id' : forall a : TL => a -> a
id' x = x

main : Int
main =
  let (w, r) = id'[(!Int, ?Int)] (new !Int) in
  let x = fork (send w 5) in
  let (y, c) = receive r in
  y
