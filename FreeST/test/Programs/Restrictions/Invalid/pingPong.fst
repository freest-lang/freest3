type PPing = forall p:(top,bot) => ?p();Wait (p+2)+0
type PPong = forall q:(top,bot) => ?q();Wait (q+2)+0
-- type PPing = ?p();Wait (p+2)+0
-- type PPong = ?q();Wait (q+2)+1

playerA : forall p:(bot,top), q:(p,top) => PPing ->[top,bot] dualof PPong 1->[p,(q+2)+1] ()
playerA ping pong =
    let (_, ping) = receive ping{p} in     --priority: p
    let pong = send () pong{q} in          --priority: q
    wait ping{p};                          --priority: p+2
    close pong{q}                          --priority: q+2

playerB : forall p:(bot,top), q:(p,top) => dualof PPing ->[top,bot] PPong 1->[p,(p+2)+0] ()
playerB ping pong =
    let (_, pong) = receive pong{q} in     --priority: q
    let ping = send () ping{p} in          --priority: p
    wait pong{q};                          --priority: q+2
    close ping{p}                          --priority: p+2

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPong () in
    fork (\_:()1-> playerA pingI pongO);
    playerB{1}{2} pingO pongI 