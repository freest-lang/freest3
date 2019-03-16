id' : forall a : TU => a -> a
id' x = x

main : Int
main =
  let w, r = id'[(!Int, ?Int)] (new !Int) in
  let x = fork (send 5 w) in
  let y, c = receive r in
  y
