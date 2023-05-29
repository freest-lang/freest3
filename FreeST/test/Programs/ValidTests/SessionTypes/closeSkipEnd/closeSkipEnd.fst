main : ()
main =
  let (w, v) = new @(Skip;EndC) () in
  fork (\_:() 1-> close w) ;
  wait v
