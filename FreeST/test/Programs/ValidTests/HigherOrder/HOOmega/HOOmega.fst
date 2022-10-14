type Omega : 1S = !Omega;End

produce : Omega -> Diverge
produce p =
  let (p', c') = new Omega in
    send p' p |> close;
    printStringLn "Producing";
    consume c'

consume : dualof Omega -> Diverge
consume c = 
    printStringLn "Consuming";
    produce (receiveAndClose @Omega c)

main : Diverge
main =
  let (p, c) = new Omega in
    fork (\_:() 1-> consume c);
    produce p
  
