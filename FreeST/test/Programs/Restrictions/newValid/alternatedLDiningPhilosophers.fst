type FirstFork = !1();?2();Close 5
type SecondFork = !3();?4();Close 6

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

evenPhilosopher : Int ->[1,bot] FirstFork ->[1,bot] dualof SecondFork 1->[3,6] ()
evenPhilosopher id left right = 
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

oddPhilosopher : Int ->[1,bot] SecondFork ->[1,bot] dualof FirstFork 1->[1,6] ()
oddPhilosopher id left right = 
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    sleep 500;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    wait right;
    close left

main : ()
main = 
    let (fw1, fr1) = new @SecondFork () in
    let (fw2, fr2) = new @FirstFork () in
    let (fw3, fr3) = new @SecondFork () in
    let (fw4, fr4) = new @FirstFork () in
    fork @() (\_:()1-> oddPhilosopher 1 fw1 fr4);
    fork @() (\_:()1-> evenPhilosopher 2 fw2 fr1);
    fork @() (\_:()1-> oddPhilosopher 3 fw3 fr2);
    evenPhilosopher 4 fw4 fr3;
    sleep 500
    -- print @String "Done!"
