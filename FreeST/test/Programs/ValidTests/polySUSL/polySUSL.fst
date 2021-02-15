id' : forall a : TL . a -> a
id' x = x

main : Int
main =
  let (w, r) = id'[(!Int, ?Int)] (new !Int) in
  let x = fork (sink (send 5 w)) in
  let (y, c) = receive r in
  y

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
