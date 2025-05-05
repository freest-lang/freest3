type Fork1 = !1();?2();Close 3
type Fork2 = !4();?5();Close 6
type Fork3 = !7();?8();Close 9

philosopher1 : Int ->[top,bot] Fork1 ->[top,bot] dualof Fork3 1->[1,3] ()
philosopher1 id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

philosopher2 : Int ->[top,bot] Fork2 ->[top,bot] dualof Fork1 1->[1,3] ()
philosopher2 id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

philosopher3 : Int ->[top,bot] Fork3 ->[top,bot] dualof Fork2 1->[1,3] ()
philosopher3 id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

main : ()
main = 
    let (fw1, fr1) = new @Fork1 () in
    let (fw2, fr2) = new @Fork2 () in
    let (fw3, fr3) = new @Fork3 () in
    fork @() (\_:()1-> philosopher1 1 fw1 fr3);
    fork @() (\_:()1-> philosopher2 2 fw2 fr1);
    philosopher3 3 fw3 fr2;
    print @String "Done!"
