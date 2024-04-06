writer : +{C: Close, D: Close} -> ()
writer c = c |> select D |> close 

reader : &{C: Wait} -> ()
reader c = match c with {
    C c -> wait c
  }
  
main : ()
main =
  let (w, r) = new @+{C: Close, D: Close} () in
  let _ = fork @() (\_:() 1-> writer w) in
  reader r
