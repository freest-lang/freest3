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

playerB : forall a:(bot,top), b:(a,top) => dualof PPing ->[top,bot] PPing 1->[a,b+2] ()
playerB =
    forall a:(bot,top), b:(a,top) =>
    \ping: dualof PPing -> 
    \pong: PPing 1->
    let ping = send () (ping{a}) in
    let (_, pong) = receive (pong{b}) in
    close ping;
    wait pong

main : ()
main =
    let (pingI, pingO) = new @PPing () in
    let (pongI, pongO) = new @PPing () in
    fork (\_:()1-> ((playerA{1}{2}) pingI pongO));
    (playerB{1}{2}) pingO pongI

-- type PPing = forall p:(bot,top) => ?p() ; Wait p+2
-- type Nothing = forall q:(bot,top) => Close q 

-- playerA : forall a:(bot,top), b:(bot,top), c:(bot,top) => PPing ->[top,bot] dualof PPing 1->[a,bot] Nothing 1->[c,b+2] ()
-- playerA =
--     forall a:(bot,top), b:(bot,top), c:(bot,top) =>
--     \ping: PPing -> 
--     \pong: dualof PPing 1->
--     \nothing: Nothing 1->
--     close (nothing{c});
--     let (_, ping) = receive (ping{a}) in
--     let pong = send () (pong{b}) in
--     wait ping;
--     close pong

-- playerB : forall a:(bot,top), b:(bot,top), c:(bot,top) => dualof PPing ->[top,bot] PPing 1->[a,bot] dualof Nothing 1->[c,b+2] ()
-- playerB =
--     forall a:(bot,top), b:(bot,top), c:(bot,top) =>
--     \ping: dualof PPing -> 
--     \pong: PPing 1->
--     \nothing: dualof Nothing 1->
--     wait (nothing{c});
--     let ping = send () (ping{a}) in
--     let (_, pong) = receive (pong{b}) in
--     close ping;
--     wait pong

-- main : ()
-- main =
--     let (pingI, pingO) = new @PPing () in
--     let (pongI, pongO) = new @PPing () in
--     let (nothingI, nothingO) = new @Nothing () in
--     fork (\_:()1-> ((playerA{2}{3}{1}) pingI pongO nothingI));
--     (playerB{2}{3}{1}) pingO pongI nothingO