main : (Int, Bool) 
main =
  let (w, r) = new @(!(Int, Bool);Close) () in
  fork (\_:()1-> send (5, True) w |> close);
  receiveAndWait @(Int, Bool) r
