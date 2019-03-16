main : Int
main =
  let w, r = new !Int in
  let w    = send 5 w in
  let x, r = receive r in
  x
