type Omega : 1S = !Omega;End

produce : Omega -> Diverge
produce p =
  let (p', c') = new Omega in
    send p' p & close;
    printStringLn "Producing";
    consume c'

consume : dualof Omega -> Diverge
consume c =
  let (c', c) = receive c in
    close c; 
    printStringLn "Consuming";
    produce c'

main : Diverge
main =
  let (p, c) = new Omega in
    fork $ consume c;
    produce p
  
