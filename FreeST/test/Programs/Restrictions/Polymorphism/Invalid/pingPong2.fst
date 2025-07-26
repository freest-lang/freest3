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

playerB : forall a:(bot,top), b:(bot,top) => dualof PPing ->[top,bot] PPing 1->[a,a+2] ()
playerB =
    forall a:(bot,top), b:(bot,top) =>
    \ping: dualof PPing -> 
    \pong: PPing 1->
    let (_, pong) = receive (pong{b}) in
    let ping = send () (ping{a}) in
    wait pong;
    close ping

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPing () in
    fork (\_:()1-> ((playerA{1}{2}) pingI pongO));
    (playerB{1}{2}) pingO pongI