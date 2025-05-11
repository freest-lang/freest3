type RecursiveSession1 = &1{Option1: RecursiveSession1}
type RecursiveSession2 = &2{Option2: RecursiveSession2}

client1 : RecursiveSession1 ->[top,bot] dualof RecursiveSession2 1->[1,2] ()
client1 (Option1 c1) c2 =
    let c2 = select Option2 c2 in
    -- print @String "running";
    client1 c1 c2

client2 : RecursiveSession2 ->[top,bot] dualof RecursiveSession1 1->[2,2] ()
client2 (Option2 c2) c1 =
    -- print @String "running";
    let c1 = select Option1 c1 in
    client2 c2 c1

start : () ->[top,bot] ()
start _ =
    let (c1, c1dual) = new @RecursiveSession1 () in
    let (c2, c2dual) = new @RecursiveSession2 () in
    fork @() (\_:() 1-> (client1 c1 (c2dual)));
    client2 c2 c1dual

main : ()
main = 
    start ()