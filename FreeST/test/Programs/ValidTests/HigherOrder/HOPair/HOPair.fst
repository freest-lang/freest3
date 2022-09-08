main : (Int, Bool) 
main =
  let (w, r) = new !(Int, Bool);End in
  fork (\_:()1-> send (5, True) w & close);
  let (p, r) = receive r in
  close r; p
