type PPing = forall p:(bot,top) => ?p() ; Wait p+2
-- type PPong = forall q:(bot,top) => ?q() ; Wait q+2
-- type PPing = ?p();Wait (p+2)+0
-- type PPong = ?q();Wait (q+2)+1

playerA : forall a:(bot,top), b:(a,top) => PPing ->[top,bot] dualof PPing 1->[a,b+2] ()
playerA =
    forall a:(bot,top), b:(a,top) =>
    \ping: PPing -> 
    \pong: dualof PPing 1->
    let (_, ping) = receive (ping{a}) in     --priority: p
    let pong = send () (pong{b}) in          --priority: q
    wait ping;                          --priority: p+2
    close pong                          --priority: q+2

playerB : forall a:(bot,top), b:(a,top) => dualof PPing ->[top,bot] PPing 1->[a,b+2] ()
playerB =
    forall a:(bot,top), b:(a,top) =>
    \ping: dualof PPing -> 
    \pong: PPing 1->
    let (_, pong) = receive (pong{b}) in     --priority: q
    let ping = send () (ping{a}) in          --priority: p
    wait pong;                          --priority: q+2
    close ping                          --priority: p+2

launch : ()
launch =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPing () in
    fork (\_:()1-> ((playerA{1}{2}) pingI pongO));
    (playerB{1}{2}) pingO pongI

main : ()
main =
    launch