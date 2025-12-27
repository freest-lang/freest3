type PingPong = forall p:(bot,top) => ?p() ; Wait p+2

player : forall a:(bot,top), b:(a,top) => PingPong ->[top,bot] dualof PingPong 1->[a,b] ()
player =
    forall a:(bot,top), b:(a,top) =>
    \ping: PingPong -> 
    \pong: dualof PingPong 1->
    let (_, ping) = receive (inst ping) in
    let pong = send () (inst pong) in
    wait ping;
    close pong

main : ()
main =
    let (pingI, pingO) = new @PingPong {1,2} () in
    let (pongI, pongO) = new @PingPong {2,2} () in
    fork (\_:()1-> ((player{priority pingI}{priority pongO}) pingI pongO));
    (player{priority pongI}{priority pingO}) pongI pingO 

--because duality does not preserve priorities, it can deadlock if I do {1}{2}/{1}{2}