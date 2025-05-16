type Hand = !1();?2();Close 3

type FirstHand = !1();?3();Close 5
type SecondHand = !2();?4();Close 6

type FirstFork = !1();?2();Close 3
type SecondFork = !4();?5();Close 6

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher : Int ->[top,bot] FirstHand 1->[top,bot] SecondHand 1->[1,6] ()
philosopher id left right =
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

-- unitaryFork : dualof Hand ->[1,6] ()
-- unitaryFork f =
--     let (_,f) = receive f in
--     let f = send () f in
--     wait f

-- fork_ : dualof Hand ->[top,bot] dualof Hand 1->[1,3] ()
-- fork_ left right =
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right;
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left

--we can cheat it using First/Second Fork, but we could do the same in the
--deadlocking version. It's just avoiding the solver so it's not a real solution.
fork_ : dualof SecondFork ->[top,bot] dualof FirstFork 1->[1,6] ()
fork_ left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left
    -- unitaryFork right;
    -- unitaryFork left

oppositeFork : dualof FirstFork ->[top,bot] dualof SecondFork 1->[1,6] ()
oppositeFork left right =
    let (_,left) = receive left in
    let left = send () left in
    wait left;
    let (_,right) = receive right in
    let right = send () right in
    wait right
    
    -- unitaryFork left;
    -- unitaryFork right

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
    sleep 500
    -- print @String "Done!"

