type Stream = forall i:(bot,top) => !i() ; !i+2() ; Stream
type Stream2 = forall u:(bot,top) => Close u

test : forall p:(bot,top) => Stream ->[top,bot] dualof Stream 1->[p,c2+2] ()
test =
    forall p:(bot,top) =>
    \c1: Stream ->
    \c2: dualof Stream 1->
    let c1 = send () (inst c1) in       --1 (inst c1)
    let (_, c2) = receive (inst c2) in  --2 (inst c2)
    let c1 = send () c1 in              --3 (c1+2)  
    let (_, c2) = receive c2 in         --4 (c2+2)
    let c1 = send () (inst c1) in       --5 (inst c1, com incremento 4)
    let (_, c2) = receive (inst c2) in  --6 (inst c2, com incremento 4)
    let c1 = send () c1 in              --7 (c1+2, com incremento 4)
    let (_, c2) = receive c2 in         --8 (c2+2, com incremento 4)
    test {priority c1} c1 c2            --aqui, c1 tem prioridade 5. Só será 9 após (inst c1) 

test2 : forall p:(bot,top), q:(bot,top) => Stream2 ->[top,bot] dualof Stream2 1->[p,q] ()
test2 =
    forall p:(bot,top), q:(bot,top) =>
    \c1: Stream2 ->
    \c2: dualof Stream2 1->
    close (inst c1);
    wait (inst c2)

main : ()
main =
    let (r1, w1) = new @Stream {1,4} () in
    let (r2, w2) = new @Stream {2,4} () in
    (test{priority r1}) r1 w2;
    let pr2 = priority r2 in
    (test{pr2}) r2 w1;

    let (r3, w3) = new @Stream2 {1,1} () in
    let (r4, w4) = new @Stream2 {1,1} () in
    fork (\_:()1-> (test2{priority r4}{priority w3}) r4 w3);
    (test2{priority r3}{priority w4}) r3 w4;
    ()