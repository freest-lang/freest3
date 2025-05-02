type FirstFork = !1();?2();Close 5
type LastFork = !3();?4();Close 6

evenPhilosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof LastFork 1->[1,3] ()
evenPhilosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

oddPhilosopher : Int ->[top,bot] LastFork ->[top,bot] dualof FirstFork 1->[1,3] ()
oddPhilosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    wait right;
    close left

main : ()
main = 
    let (fw1, fr1) = new @LastFork () in
    let (fw2, fr2) = new @FirstFork () in
    let (fw3, fr3) = new @LastFork () in
    let (fw4, fr4) = new @FirstFork () in
    fork @() (\_:()1-> oddPhilosopher 1 fw1 fr4);
    fork @() (\_:()1-> evenPhilosopher 2 fw2 fr1);
    fork @() (\_:()1-> oddPhilosopher 3 fw3 fr2);
    evenPhilosopher 4 fw4 fr3;
    print @String "Done!"
