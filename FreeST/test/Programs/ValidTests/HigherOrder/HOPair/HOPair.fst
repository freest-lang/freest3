main : (Int, Bool) 
main =
  let (w, r) = new !(Int, Bool);End in
  fork $ send (5, True) w;
  let (p, r) = receive r in
  close r; p
