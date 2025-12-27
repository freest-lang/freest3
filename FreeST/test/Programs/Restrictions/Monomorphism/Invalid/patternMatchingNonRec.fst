type SessionT1 = &p{Option1: Close p+2}
type SessionT2 = &q{Option2: Close q+2}

client1 : SessionT1 ->[top,bot] dualof SessionT2 1->[p,q+2] ()
client1 (Option1 c1) c2 =
    let c2 = select Option2 c2 in
    close c1;
    close c2

client2 : SessionT2 ->[top,bot] dualof SessionT1 1->[q,p+2] ()
client2 (Option2 c2) c1 =
    let c1 = select Option1 c1 in
    close c2;
    close c1  

main : ()
main =
    let (c1, c1dual) = new @SessionT1 () in
    let (c2, c2dual) = new @SessionT2 () in
    fork (\_:() 1-> client1 c1 c2dual);
    client2 c2 c1dual