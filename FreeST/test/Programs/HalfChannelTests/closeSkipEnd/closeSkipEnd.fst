main : ()
main =
  let (w, v) = new @(Skip;Close) () in
  fork (\_:() 1-> close w) ;
  wait v
