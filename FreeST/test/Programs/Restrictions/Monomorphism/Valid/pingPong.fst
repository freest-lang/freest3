type Ping = ?p();Wait p+2
type Pong = ?q();Wait q+2

a : Ping ->[top,bot] dualof Pong 1->[p,q+2] ()
a ping pong =
    let (_, ping) = receive ping in
    let pong = send () pong in
    wait ping;
    close pong

b : dualof Ping ->[top,bot] Pong 1->[p,q+2] ()
b ping pong =
    let ping = send () ping in
    let (_, pong) = receive pong in
    close ping;
    wait pong

main : ()
main =
    let (pingI, pingO) = new @Ping () in
    let (pongI, pongO) = new @Pong () in
    fork @() (\_:()1-> a pingI pongO);
    b pingO pongI 