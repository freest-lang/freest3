main : Int
main =
  let (w, r) = new @!Int;Skip () in
  fork (send 5 w);
  let (n, r1) = receive r in
  n
