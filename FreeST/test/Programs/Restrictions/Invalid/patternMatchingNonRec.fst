type Session1 = &1{Option1: Close 3}
type Session2 = &2{Option2: Close 4}

client1 : Session1 ->[top,bot] dualof Session2 1->[1,4] ()
client1 (Option1 c1) c2 =
    print @String "running";
    let c2 = select Option2 c2 in
    close c1;
    close c2
    -- client1 c1 c2

client2 : Session2 ->[top,bot] dualof Session1 1->[2,4] ()
client2 (Option2 c2) c1 =
    print @String "running";
    let c1 = select Option1 c1 in
    close c1;
    close c2
    -- client2 c2 c1

start : () ->[top,bot] ()
start _ =
    let (c1, c1dual) = new @Session1 () in
    let (c2, c2dual) = new @Session2 () in
    fork @() (\_:() 1-> (client1 c1 (c2dual)));
    client2 c2 c1dual

main : ()
main = 
    start ()