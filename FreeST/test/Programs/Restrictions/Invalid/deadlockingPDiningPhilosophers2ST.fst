type Hand = !10();?11();Close 12

type FirstHand = !1();?2();Close 3
type SecondHand = !4();?5();Close 6

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

fork_ : dualof SecondHand ->[top,bot] dualof FirstHand 1->[1,6] ()
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