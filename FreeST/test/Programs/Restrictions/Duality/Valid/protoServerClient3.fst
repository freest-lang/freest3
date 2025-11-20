type Stream = forall i:(bot,top) => !i() ; !i+2() ; Stream

test : forall p:(bot,top) => Stream ->[top,bot] dualof Stream 1->[i,c2+2] ()
test =
    forall p:(bot,top) =>
    \c1: Stream ->
    \c2: dualof Stream 1->
    let c1 = send () (inst c1) in
    let (_, c2) = receive (inst c2) in
    let c1 = send () c1 in
    let (_, c2) = receive c2 in
    let c1 = send () (inst c1) in
    let (_, c2) = receive (inst c2) in
    let c1 = send () c1 in
    let (_, c2) = receive c2 in
    test {priority c1} c1 c2

main : ()
main =
    let (r1, w1) = new @Stream {1,4} () in
    let (r2, w2) = new @Stream {2,4} () in
    test {priority r1} r1 w2;
    let pr2 = priority r2 in
    test {pr2} r2 w1;
    ()