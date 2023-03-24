writer : +{A: Skip, B: Skip, C: Skip};End -> ()
writer c = c |> select C |> close 

reader : &{A: Skip, B: Skip};End -> ()
reader (A c) = close c 
reader (B c) = close c 
  
main : ()
main =
  let (w, r) = new @(+{A: Skip, B: Skip};End) () in
  let _ = fork @() (\_:() 1-> writer w) in
  reader r
