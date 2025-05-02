type Hand = !1Int;?2();Close 3

type FirstHand = !1Int;?3();Close 5
type SecondHand = !2Int;?4();Close 6

philosopher : Int ->[top,bot] FirstHand 1->[top,bot] SecondHand 1->[1,6] ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

unitaryFork : !3();Wait 6->[3,6] ()
unitaryFork f =
    let f = send () f in
    wait f

fork_ : dualof SecondHand ->[top,bot] dualof FirstHand 1->[1,3] ()
fork_ left right =
    let (idr, right) = receive right in
    let (idl, left) = receive left in
    if(idl < idr) 
    then
        unitaryFork left;
        unitaryFork right
    else
        unitaryFork right;
        unitaryFork left

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
    fork @() (\_ : () 1-> fork_ f6 f5);
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    philosopher 3 p5 p4;
    print @String "Done!"

