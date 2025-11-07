type Stream = forall p:(bot,top) => Close p

test : forall a:(bot,top), b:(a,top) => Stream ->[top,bot] dualof Stream 1->[a,b] ()
test =
    forall a:(bot,top), b:(a,top) =>
    \c1: Stream ->
    \c2: dualof Stream 1->
    close (c1{a}); --1
    wait (c2{b})   --2

main : ()
main =
    let (r1, w1) = new @Stream () in --(1,2)
    let (r2, w2) = new @Stream () in --(2,2)
    (test{1}{2}) r1 w2;
    wait (w1{1});  --1
    close (r2{2}) --2