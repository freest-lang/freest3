main : Int
main =
  let (w, r) = new !Int;End in
  let w    = send 5 w in
  let (x, r) = receive r in
  close w; close r; 
  x
