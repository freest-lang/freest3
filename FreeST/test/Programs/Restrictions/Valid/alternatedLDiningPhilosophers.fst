type Fork = !1();?2();Close 3

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher : Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[1,3] ()
philosopher id left right = 
    sleep 1000;
    -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    if(even id) 
    then
        let left = send () left in
        let (_,left) = receive left in
        let (_,right) = receive right in
        let right = send () right in
        sleep 1000;
        -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        close left;
        wait right
    else
        let (_,right) = receive right in
        let right = send () right in
        let left = send () left in
        let (_,left) = receive left in
        sleep 1000;
        -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        wait right;
        close left

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    let (fw4, fr4) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr4);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    fork @() (\_:()1-> philosopher 3 fw3 fr2);
    -- philosopher 3 fw3 fr2;
    philosopher 4 fw4 fr3;
    sleep 1000;
    ()
    -- print @String "Done!"
