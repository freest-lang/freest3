type Stream = forall i:(bot,top) => !i() ; Stream

-- client : forall p:(bot,top) => Stream ->[top,bot] dualof Stream 1->[p,top] ()
-- client c1 c2 =
--     -- forall p:(bot,top) =>
--     -- \c1: Stream ->
--     -- \c2: dualof Stream 1->
--     let c1 = send () (inst c1) in
--     let (_, c2) = receive (inst c2) in
--     let c1 = send () (inst c1) in
--     let (_, c2) = receive (inst c2) in
--     client {lowest c1} c1 c2

-- server : forall p:(bot,top) => dualof Stream ->[top,bot] Stream 1->[p,top] ()
-- server c1 c2 =
--     -- forall p:(bot,top) =>
--     -- \c1: dualof Stream ->
--     -- \c2: Stream 1->
--     let (_, c1) = receive (inst c1) in
--     let c2 = send () (inst c2) in
--     let (_, c1) = receive (inst c1) in
--     let c2 = send () (inst c2) in
--     server {highest c1} c1 c2

sleep : Int ->[top,bot] Int ->[top,bot] ()
sleep n m = if n == 0 || m == 0 then () else sleep (n-1) (m-1)

test : forall p:(bot,top) => Stream ->[top,bot] dualof Stream 1->[i,c2] ()
test =
    forall p:(bot,top) =>
    \c1: Stream ->
    \c2: dualof Stream 1->
    let c1 = send () (inst c1) in
    let (_, c2) = receive (inst c2) in
    let c1 = send () (inst c1) in
    let (_, c2) = receive (inst c2) in
    ()

main : ()
main =
    -- let ps = (1,2) in
    -- let qs = (2,2) in
    let (r1, w1) = new @Stream {1,2} () in
    let (r2, w2) = new @Stream {2,2} () in
    test {priority r1} r1 w2;
    -- let r1 = send () (inst r1) in
    -- let (_, w1) = receive (inst w1) in
    let r2 = priority r2 in
    -- sleep 500 500;
    -- close r1;
    -- wait w1;
    -- let (r2, w2) = new @Stream qs in
    -- fork (\_:()1-> (client {lowest r1} r1 w2));
    -- server {lowest w1} w1 r2
    ()