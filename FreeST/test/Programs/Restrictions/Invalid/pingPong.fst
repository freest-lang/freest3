type PPing = forall p:(top,bot) => ?p();Wait (p+2)+0
type PPong = forall q:(top,bot) => ?q();Wait (q+2)+0
-- type PPing = ?p();Wait (p+2)+0
-- type PPong = ?q();Wait (q+2)+1

playerA : forall p:(bot,top), q:(p,top) => PPing ->[top,bot] dualof PPong 1->[p,(q+2)+1] ()
playerA ping pong =
    let (_, ping) = receive ping in     --priority: p
    let pong = send () pong in          --priority: q
    wait ping;                          --priority: |{\color{codegreen}p+2}|
    close pong                          --priority: |{\color{codegreen}q+2}|

playerB : forall p:(bot,top), q:(p,top) => dualof PPing ->[top,bot] PPong 1->[p,(p+2)+0] ()
playerB ping pong =
    let (_, pong) = receive pong in     --priority: q
    let ping = send () ping in          --priority: p
    wait pong;                          --priority: |{\color{codegreen}q+2}|
    close ping                          --priority: |{\color{codegreen}p+2}|

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPong () in
    fork (\_:()1-> playerA pingI pongO);
    playerB{1}{2} pingO pongI 