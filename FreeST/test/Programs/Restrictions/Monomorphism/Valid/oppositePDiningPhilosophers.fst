type Hand1 = !a Int;?b();Close c
type Hand2 = !d Int;?e();Close f
type Hand3 = !g Int;?h();Close i
type Hand4 = !j Int;?k();Close l
type Hand5 = !m Int;?n();Close o
type Hand6 = !p Int;?q();Close r
type Hand7 = !s Int;?t();Close u
type Hand8 = !v Int;?w();Close x

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[top,bot] Hand1 1->[top,bot] Hand8 1->[a,x] ()
philosopher1 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher2 : Int ->[top,bot] Hand3 1->[top,bot] Hand2 1->[g,f] ()
philosopher2 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher3 : Int ->[top,bot] Hand5 1->[top,bot] Hand4 1->[m,l] ()
philosopher3 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

philosopher4 : Int ->[top,bot] Hand7 1->[top,bot] Hand6 1->[s,r] ()
philosopher4 id left right =
    sleep 500;
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    sleep 500;
    close left;
    close right

fork1 : dualof Hand2 ->[top,bot] dualof Hand1 1->[d,f] ()
fork1 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let right = send () right in
    wait right;
    let left = send () left in
    wait left

fork2 : dualof Hand4 ->[top,bot] dualof Hand3 1->[j,l] ()
fork2 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let right = send () right in
    wait right;
    let left = send () left in
    wait left

fork3 : dualof Hand6 ->[top,bot] dualof Hand5 1->[p,r] ()
fork3 left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let right = send () right in
    wait right;
    let left = send () left in
    wait left

oppositeFork : dualof Hand8 ->[top,bot] dualof Hand7 1->[v,u] ()
oppositeFork left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    let left = send () left in
    wait left;
    let right = send () right in
    wait right

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
    fork @() (\_ : () 1-> oppositeFork f8 f7);
    fork @() (\_ : () 1-> philosopher1 1 p1 p8);
    fork @() (\_ : () 1-> philosopher2 2 p3 p2);
    fork @() (\_ : () 1-> philosopher3 3 p5 p4);
    philosopher4 4 p7 p6;
    sleep 500 