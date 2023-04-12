mathServer : &{Negate: ?Int;!Int, Add: ?Int;?Int;!Int};End -> End
mathServer (Negate c) = 
  let (n, c) = receive c in
  send (-n) c
mathServer (Add c) =
  let (n1, c) = receive c in
  let (n2, c) = receive c in
  send (n1 + n2) c

main : Int
main =
  let (r,w) = new @(&{Negate: ?Int;!Int, Add: ?Int;?Int;!Int};End) () in
  fork (\_:() 1-> mathServer r |> close) ;
  let (x, w) = receive (send 5 (select Negate w)) in
  close w;
  x
