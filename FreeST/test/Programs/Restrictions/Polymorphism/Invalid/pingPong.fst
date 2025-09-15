type PPing = forall p:(bot,top) => ?p() ; Wait p+2

playerA : forall a:(bot,top), b:(a,top) => PPing ->[top,bot] dualof PPing 1->[a,b+2] ()
playerA =
    forall a:(bot,top), b:(a,top) =>
    \ping: PPing -> 
    \pong: dualof PPing 1->
    let (_, ping) = receive (ping{a}) in
    let pong = send () (pong{b}) in
    wait ping;
    close pong

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPing () in
    fork (\_:()1-> ((playerA{1}{2}) pingI pongO));
    (playerA{2}{1}) pongI pingO 

--because duality does not preserve priorities, it can deadlock if I do {1}{2}/{1}{2}