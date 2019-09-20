main : Int
main =
  let (w, r) = new !Int in
  let w    = send w 5 in
  let (x, r) = receive r in
  x
