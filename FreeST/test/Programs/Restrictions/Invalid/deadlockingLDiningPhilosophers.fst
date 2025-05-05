type Fork = !1();?2();Close 3

-- type FirstFork = !1();?3();Close 5
-- type SecondFork = !2();?4();Close 6

-- philosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof SecondFork 1->[1,6] ()
-- philosopher id left right = 
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
--     let left = send () left in
--     let (_,right) = receive right in
--     let (_,left) = receive left in
--     let right = send () right in
--     putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     wait right

philosopher : Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[1,3] ()
philosopher id left right = 
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
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2;
    print @String "Done!"
