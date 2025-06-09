type Hand1 = !1Int;?2();Close 3
type Hand2 = !4Int;?5();Close 6
type Hand3 = !7Int;?8();Close 9
type Hand4 = !10Int;?11();Close 12
type Hand5 = !13Int;?14();Close 15
type Hand6 = !16Int;?17();Close 18
type Hand7 = !19Int;?20();Close 21
type Hand8 = !22Int;?23();Close 24

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[top,bot] Hand1 1->[top,bot] Hand8 1->[1,24] ()
philosopher1 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher2 : Int ->[top,bot] Hand3 1->[top,bot] Hand2 1->[7,6] ()
philosopher2 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher3 : Int ->[top,bot] Hand5 1->[top,bot] Hand4 1->[13,12] ()
philosopher3 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher4 : Int ->[top,bot] Hand7 1->[top,bot] Hand6 1->[19,18] ()
philosopher4 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

fork1 : dualof Hand2 ->[top,bot] dualof Hand1 1->[4,3] ()
fork1 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let left = send () left in
    wait left;
    let right = send () right in
    wait right

fork2 : dualof Hand4 ->[top,bot] dualof Hand3 1->[10,12] ()
fork2 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let right = send () right in
    wait right;
    let left = send () left in
    wait left

fork3 : dualof Hand6 ->[top,bot] dualof Hand5 1->[16,15] ()
fork3 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let left = send () left in
    wait left;
    let right = send () right in
    wait right

fork4 : dualof Hand8 ->[top,bot] dualof Hand7 1->[22,24] ()
fork4 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let right = send () right in
    wait right;
    let left = send () left in
    wait left

main : ()
main =
    let (p1, f1) = new @Hand1 () in
    let (p2, f2) = new @Hand2 () in
    let (p3, f3) = new @Hand3 () in
    let (p4, f4) = new @Hand4 () in
    let (p5, f5) = new @Hand5 () in
    let (p6, f6) = new @Hand6 () in
    let (p7, f7) = new @Hand7 () in
    let (p8, f8) = new @Hand8 () in
    fork @() (\_ : () 1-> fork1 f2 f1);
    fork @() (\_ : () 1-> fork2 f4 f3);
    fork @() (\_ : () 1-> fork3 f6 f5);
    fork @() (\_ : () 1-> fork4 f8 f7);
    fork @() (\_ : () 1-> philosopher1 1 p1 p8);
    fork @() (\_ : () 1-> philosopher2 2 p3 p2);
    fork @() (\_ : () 1-> philosopher3 3 p5 p4);
    philosopher4 4 p7 p6;
    sleep 500 