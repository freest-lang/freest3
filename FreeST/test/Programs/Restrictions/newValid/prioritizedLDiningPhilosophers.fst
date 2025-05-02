type Fork = !1Int;ForkTail
type ForkTail = ?3();Close 6

type FirstFork = !1Int;ForkTail
type SecondFork = !2Int;ForkTail

type FirstForkTail = ?3();Close 5
type SecondForkTail = ?4();Close 6

lesserPhilosopher : Int ->[top,bot] FirstForkTail ->[top,bot] dualof SecondForkTail 1->[3,6] ()
lesserPhilosopher id left right = 
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

greaterPhilosopher : Int ->[top,bot] SecondForkTail ->[top,bot] dualof FirstForkTail 1->[3,6] ()
greaterPhilosopher id left right = 
    let right = send () right in
    let (_,left) = receive left in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    wait right;
    close left

philosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof SecondFork 1->[1,6] ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let (idr,right) = receive right in
    if(id < idr) 
    then
        lesserPhilosopher id left right
    else
        greaterPhilosopher id left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2;
    print @String "Done!"
