writer : +{A: Skip, B: Skip};Close -> ()
writer c = c |> select A |> close

reader : &{A: Skip, B: Skip, C:Skip};Wait -> ()
reader (A c) = wait c 
reader (B c) = wait c
  
main : ()
main =
  let (w, r) = new @(+{A: Skip, B: Skip};Close) () in
  fork @() (\_:() 1-> writer w);
  reader r
