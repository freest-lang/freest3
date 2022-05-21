id' : forall a : 1T . a -> a
id' x = x

main : Int
main =
  let (w, r) = id' @(!Int, ?Int) (new !Int) in
  let x = fork @Skip \_:() 1-> send 5 w in
  let (y, c) = receive r in
  y
