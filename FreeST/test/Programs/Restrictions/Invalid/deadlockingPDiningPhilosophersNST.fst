type Hand1 = !a();?b();Close c
type Hand2 = !d();?e();Close f
type Hand3 = !g();?h();Close i
type Hand4 = !j();?k();Close l
type Hand5 = !m();?n();Close o
type Hand6 = !p();?q();Close r

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[top,bot] Hand1 1->[top,bot] Hand6 1->[a,r] ()
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

philosopher2 : Int ->[top,bot] Hand3 1->[top,bot] Hand2 1->[g,f] ()
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

philosopher3 : Int ->[top,bot] Hand5 1->[top,bot] Hand4 1->[m,l] ()
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

fork1 : dualof Hand2 ->[top,bot] dualof Hand1 1->[d,f] ()
fork1 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left

fork2 : dualof Hand4 ->[top,bot] dualof Hand3 1->[j,l] ()
fork2 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left

fork3 : dualof Hand6 ->[top,bot] dualof Hand5 1->[p,r] ()
fork3 left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
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
    fork @() (\_ : () 1-> fork1 f2 f1);
    fork @() (\_ : () 1-> fork2 f4 f3);
    fork @() (\_ : () 1-> fork3 f6 f5);
    fork @() (\_ : () 1-> philosopher1 1 p1 p6);
    fork @() (\_ : () 1-> philosopher2 2 p3 p2);
    philosopher3 3 p5 p4;
    sleep 500 
    -- print @String "Done!"