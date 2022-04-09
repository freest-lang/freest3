main : ((Int, Bool), Skip) 
main =
  let (w, r) = new !(Int, Bool) in
  fork $ send (5, True) w;
  receive r
