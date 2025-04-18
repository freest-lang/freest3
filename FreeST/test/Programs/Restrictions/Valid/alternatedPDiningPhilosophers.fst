type Hand = !1Int;?2();Close 3

philosopher : Int ->[top,bot] Hand 1->[top,bot] Hand 1->[1,3] ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let right = send id right in
    let (_,left) = receive left in
    let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

unitaryFork : !2();Wait 3->[2,3] ()
unitaryFork f =
    let f = send () f in
    wait f

fork_ : dualof Hand ->[top,bot] dualof Hand 1->[1,3] ()
fork_ left right =
    let (id,right) = receive right in
    let (_,left) = receive left in
    if(even id) 
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

