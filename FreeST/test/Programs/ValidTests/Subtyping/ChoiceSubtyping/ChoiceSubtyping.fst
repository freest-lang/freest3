writer : +{C: Skip};Close -> ()
writer c = c |> select C |> close 

reader : &{C: Skip, D: Skip};Wait -> ()
reader (C c) = wait c 
reader (D c) = wait c 
  
main : ()
main =
  let (w, r) = new @(+{C: Skip};Close) () in
  fork @() (\_:() 1-> writer w);
  reader r
