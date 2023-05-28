type Omega : 1S = !Omega;EndC

produce : Omega -> Diverge
produce p =
  let (p', c') = new @Omega () in
    send p' p |> close;
    putStrLn "Producing";
    consume' c'

consume' : dualof Omega -> Diverge
consume' c = 
    putStrLn "Consuming";
    produce (receiveAndWait @Omega c)

main : Diverge
main =
  let (p, c) = new @Omega () in
    fork (\_:() 1-> consume' c);
    produce p
  
