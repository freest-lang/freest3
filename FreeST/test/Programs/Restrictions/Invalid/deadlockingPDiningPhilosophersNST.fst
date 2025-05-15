type Hand = !100();?101();Close 102

type Hand1 = !1();?2();Close 3
type Hand2 = !4();?5();Close 6
type Hand3 = !7();?8();Close 9
type Hand4 = !10();?11();Close 12
type Hand5 = !13();?14();Close 15
type Hand6 = !16();?17();Close 18

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[1,bot] Hand1 1->[1,bot] Hand6 1->[16,18] ()
philosopher1 id left right =
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

philosopher2 : Int ->[4,bot] Hand3 1->[4,bot] Hand2 1->[4,9] ()
philosopher2 id left right =
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

philosopher3 : Int ->[10,bot] Hand5 1->[10,bot] Hand4 1->[10,15] ()
philosopher3 id left right =
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

-- unitaryFork : dualof Hand ->[1,18] ()
-- unitaryFork f =
--     let (_,f) = receive f in
--     let f = send () f in
--     wait f

fork1 : dualof Hand2 ->[1,bot] dualof Hand1 1->[1,6] ()
fork1 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left
    -- unitaryFork right;
    -- unitaryFork left

fork2 : dualof Hand4 ->[7,bot] dualof Hand3 1->[7,12] ()
fork2 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left
    -- unitaryFork right;
    -- unitaryFork left

fork3 : dualof Hand6 ->[13,bot] dualof Hand5 1->[13,18] ()
fork3 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left
    -- unitaryFork right;
    -- unitaryFork left

main : ()
main =
    let (p1, f1) = new @Hand1 () in
    let (p2, f2) = new @Hand2 () in
    let (p3, f3) = new @Hand3 () in
    let (p4, f4) = new @Hand4 () in
    let (p5, f5) = new @Hand5 () in
    let (p6, f6) = new @Hand6 () in
    fork @() (\_ : () 1-> fork1 f2 f1);
    fork @() (\_ : () 1-> fork2 f4 f3);
    fork @() (\_ : () 1-> fork3 f6 f5);
    fork @() (\_ : () 1-> philosopher1 1 p1 p6);
    fork @() (\_ : () 1-> philosopher2 2 p3 p2);
    philosopher3 3 p5 p4;
    sleep 500 
    -- print @String "Done!"