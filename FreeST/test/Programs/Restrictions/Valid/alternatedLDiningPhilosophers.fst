type FirstFork = !p();?p+1();Close p+4
type SecondFork = !p+2();?p+3();Close p+5

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

evenPhilosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof SecondFork 1->[p,p+5] ()
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

oddPhilosopher : Int ->[top,bot] SecondFork ->[top,bot] dualof FirstFork 1->[p+2,p+5] ()
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
