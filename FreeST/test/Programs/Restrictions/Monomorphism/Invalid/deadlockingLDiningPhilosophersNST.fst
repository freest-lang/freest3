type Fork1 = !a();?b();Close c
type Fork2 = !d();?e();Close f
type Fork3 = !g();?h();Close i

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[top,bot] Fork1 ->[top,bot] dualof Fork3 1->[a,i] ()
philosopher1 id left right = 
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

philosopher2 : Int ->[top,bot] Fork2 ->[top,bot] dualof Fork1 1->[d,c] ()
philosopher2 id left right =
    sleep 500; 
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

philosopher3 : Int ->[top,bot] Fork3 ->[top,bot] dualof Fork2 1->[g,f] ()
philosopher3 id left right = 
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
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
    sleep 500
    -- print @String "Done!"
