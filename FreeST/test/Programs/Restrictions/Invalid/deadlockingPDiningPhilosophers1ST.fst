type Hand = !1();?2();Close 3

philosopher : Int ->[top,bot] Hand 1->[top,bot] Hand 1->[1,3] ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

fork_ : dualof Hand ->[top,bot] dualof Hand 1->[1,3] ()
fork_ left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left

main : ()
main =
    let (p1, f1) = new @Hand () in
    let (p2, f2) = new @Hand () in
    let (p3, f3) = new @Hand () in
    let (p4, f4) = new @Hand () in
    let (p5, f5) = new @Hand () in
    let (p6, f6) = new @Hand () in
    let (p7, f7) = new @Hand () in
    let (p8, f8) = new @Hand () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    fork @() (\_ : () 1-> fork_ f6 f5);
    fork @() (\_ : () 1-> fork_ f8 f7);
    fork @() (\_ : () 1-> philosopher 1 p1 p8);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    fork @() (\_ : () 1-> philosopher 3 p5 p4);
    philosopher 4 p7 p6;
    print @String "Done!"

-- type Hand = !1Int;?2();Close 3

-- type FirstHand = !1Int;?3();Close 5
-- type SecondHand = !2Int;?4();Close 6

-- philosopher : Int ->[top,bot] FirstHand 1->[top,bot] SecondHand 1->[1,6] ()
-- philosopher id left right =
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
--     let left = send id left in
--     let right = send id right in
--     let (_, left) = receive left in
--     let (_, right) = receive right in
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     close right

-- unitaryFork : dualof Hand ->[1,6] ()
-- unitaryFork f =
--     let (_,f) = receive f in
--     let f = send () f in
--     wait f

-- fork_ : dualof SecondHand ->[top,bot] dualof FirstHand 1->[1,6] ()
-- fork_ left right =
--     unitaryFork right;
--     unitaryFork left

-- main : ()
-- main =
--     let (p1, f1) = new @Hand () in
--     let (p2, f2) = new @Hand () in
--     let (p3, f3) = new @Hand () in
--     let (p4, f4) = new @Hand () in
--     let (p5, f5) = new @Hand () in
--     let (p6, f6) = new @Hand () in
--     fork @() (\_ : () 1-> fork_ f2 f1);
--     fork @() (\_ : () 1-> fork_ f4 f3);
--     fork @() (\_ : () 1-> fork_ f6 f5);
--     fork @() (\_ : () 1-> philosopher 1 p1 p6);
--     fork @() (\_ : () 1-> philosopher 2 p3 p2);
--     philosopher 3 p5 p4;
--     print @String "Done!"





-- type Hand = !1();?2();Close 3

-- type FirstHand = !1();?3();Close 5
-- type SecondHand = !2();?4();Close 6
-- type ThirdHand = !7();?9();Close 11
-- type FourthHand = !8();?10();Close 12
-- type FifthHand = !13();?15();Close 17
-- type SixthHand = !14();?16();Close 18

-- philosopher1 : Int ->[top,bot] FirstHand 1->[top,bot] SecondHand 1->[1,6] ()
-- philosopher1 id left right =
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
--     let left = send () left in
--     let right = send () right in
--     let (_, left) = receive left in
--     let (_, right) = receive right in
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     close right

-- philosopher2 : Int ->[top,bot] ThirdHand 1->[top,bot] FourthHand 1->[7,12] ()
-- philosopher2 id left right =
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
--     let left = send () left in
--     let right = send () right in
--     let (_, left) = receive left in
--     let (_, right) = receive right in
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     close right

-- philosopher3 : Int ->[top,bot] FifthHand 1->[top,bot] SixthHand 1->[13,18] ()
-- philosopher3 id left right =
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
--     let left = send () left in
--     let right = send () right in
--     let (_, left) = receive left in
--     let (_, right) = receive right in
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     close right

-- fork1 : dualof FourthHand ->[top,bot] dualof FirstHand 1->[1,12] ()
-- fork1 left right =
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right;
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left

-- fork2 : dualof SixthHand ->[top,bot] dualof ThirdHand 1->[7,18] ()
-- fork2 left right =
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right;
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left

-- fork3 : dualof FifthHand ->[top,bot] dualof SecondHand 1->[2,17] ()
-- fork3 left right =
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right;
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left

-- main : ()
-- main =
--     let (p1, f1) = new @Hand () in
--     let (p2, f2) = new @Hand () in
--     let (p3, f3) = new @Hand () in
--     let (p4, f4) = new @Hand () in
--     let (p5, f5) = new @Hand () in
--     let (p6, f6) = new @Hand () in
--     fork @() (\_ : () 1-> fork1 f2 f1);
--     fork @() (\_ : () 1-> fork2 f4 f3);
--     fork @() (\_ : () 1-> fork3 f6 f5);
--     fork @() (\_ : () 1-> philosopher1 1 p1 p6);
--     fork @() (\_ : () 1-> philosopher2 2 p3 p2);
--     philosopher3 3 p5 p4;
--     print @String "Done!"