type PPing = forall p:(bot,top) => ?p() ; Wait p+2
type PPong = forall q:(bot,top) => ?q() ; Wait q+2
-- type PPing = ?p();Wait (p+2)+0
-- type PPong = ?q();Wait (q+2)+1

playerA : forall p:(bot,top), q:(p,top) => PPing ->[top,bot] dualof PPong 1->[p,q+2] ()
playerA =
    forall p:(bot,top), q:(p,top) =>
    \ping: PPing -> 
    \pong: dualof PPong 1->
    let (_, ping) = receive (ping{p}) in     --priority: p
    let pong = send () (pong{q}) in          --priority: q
    wait ping;                          --priority: p+2
    close pong                          --priority: q+2

playerB : forall p:(bot,top), q:(p,top) => dualof PPing ->[top,bot] PPong 1->[p,p+2] ()
playerB =
    forall p:(bot,top), q:(p,top) =>
    \ping: dualof PPing -> 
    \pong: PPong 1->
    let (_, pong) = receive (pong{q}) in     --priority: q
    let ping = send () (ping{p}) in          --priority: p
    wait pong;                          --priority: q+2
    close ping                          --priority: p+2

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPong () in
    fork (\_:()1-> (playerA{1}{2}) pingI pongO);
    (playerB{1}{2}) pingO pongI 