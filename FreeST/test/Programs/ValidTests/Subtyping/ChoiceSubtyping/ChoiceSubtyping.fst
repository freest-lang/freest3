writer : +{C: Skip};End -> ()
writer c = c |> select C |> close 

reader : &{C: Skip, D: Skip};End -> ()
reader (C c) = close c 
reader (D c) = close c 
  
main : ()
main =
  let (w, r) = new @(+{C: Skip};End) () in
  let _ = fork @() (\_:() 1-> writer w) in
  reader r
