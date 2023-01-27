main : Bool
main =
  let (w, v) = new @(Skip;End) () in
  fork (\_:() 1-> close w) ; close v ;
  True
