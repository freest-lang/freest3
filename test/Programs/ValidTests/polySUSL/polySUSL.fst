id' : forall a : TU => a -> a
id' x = x

start : Int
start =
  let w, r = id'[(!Int, ?Int)] (new !Int) in
  let x = fork (send 5 w) in
  let y, c = receive r in
  y
