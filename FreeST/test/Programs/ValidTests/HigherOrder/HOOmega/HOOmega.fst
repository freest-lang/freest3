type Omega : SL = !Omega
type Diverge = Char -- Pick your choice

produce : Omega -> Diverge
produce p =
  let (p', c') = new Omega in
    send p' p;
    printStringLn "Producing";
    consume c'

consume : dualof Omega -> Diverge
consume c =
  let (c', _) = receive c in
    printStringLn "Consuming";
    produce c'

main : Diverge
main =
  let (p, c) = new Omega in
    fork $ consume c;
    produce p
  
