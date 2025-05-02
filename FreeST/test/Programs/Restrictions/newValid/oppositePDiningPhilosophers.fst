type Hand = !1();?2();Close 3

type FirstHand = !1();?3();Close 5
type SecondHand = !2();?4();Close 6

philosopher : Int ->[top,bot] FirstHand 1->[top,bot] SecondHand 1->[1,6] ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

unitaryFork : dualof Hand ->[1,6] ()
unitaryFork f =
    let (_,f) = receive f in
    let f = send () f in
    wait f

-- fork_ : dualof Hand ->[top,bot] dualof Hand 1->[1,3] ()
-- fork_ left right =
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right;
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left

fork_ : dualof SecondHand ->[top,bot] dualof FirstHand 1->[1,6] ()
fork_ left right =
    unitaryFork right;
    unitaryFork left

oppositeFork : dualof FirstHand ->[top,bot] dualof SecondHand 1->[1,6] ()
oppositeFork left right =
    unitaryFork left;
    unitaryFork right

main : ()
main =
    let (p1, f1) = new @Hand () in
    let (p2, f2) = new @Hand () in
    let (p3, f3) = new @Hand () in
    let (p4, f4) = new @Hand () in
    let (p5, f5) = new @Hand () in
    let (p6, f6) = new @Hand () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    -- fork @() (\_ : () 1-> fork_ f5 f6); --here, the forks are passed in the opposite order
    fork @() (\_ : () 1-> oppositeFork f6 f5); --here, the forks are passed in the opposite order
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    philosopher 3 p5 p4;
    print @String "Done!"

