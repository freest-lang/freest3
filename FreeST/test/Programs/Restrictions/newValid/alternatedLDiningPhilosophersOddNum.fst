type Fork = !0();?0();Close 0

type FirstFork = !1();?2();Close 5
type LastFork = !3();?4();Close 6

evenPhilosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof LastFork 1->[1,6] ()
evenPhilosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

oddPhilosopher : Int ->[top,bot] LastFork ->[top,bot] dualof FirstFork 1->[1,6] ()
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
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> oddPhilosopher 1 fw1 fr3);
    fork @() (\_:()1-> evenPhilosopher 2 fw2 fr1);
    oddPhilosopher 3 fw3 fr2;
    print @String "Done!"
