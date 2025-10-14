type PPing = forall p:(bot,top) => ?p() ; Wait p+2

playerA : forall a:(bot,top), b:(a,top) => PPing ->[top,bot] dualof PPing 1->[a,b+2] ()
playerA =
    forall a:(bot,top), b:(a,top) =>
    \ping: PPing -> 
    \pong: dualof PPing 1->
    let pong = send () (pong{a}) in
    let (_, ping) = receive (ping{b}) in
    close pong;
    wait ping

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPing () in
    fork (\_:()1-> ((playerA{1}{2}) pingI pongO));
    (playerA{2}{1}) pongI pingO 

--because duality does not preserve priorities, it can deadlock if I do {1}{2}/{1}{2}