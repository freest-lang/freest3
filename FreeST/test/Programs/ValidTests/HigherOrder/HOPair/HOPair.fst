main : (Int, Bool) 
main =
  let (w, r) = new @!(Int, Bool);End () in
  fork (\_:()1-> send (5, True) w |> close);
  receiveAndClose @(Int, Bool) r
