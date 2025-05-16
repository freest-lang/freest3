--I think this one is just impossible because of the first exchange. We always run into 
--a situation where a<b<c<a. In the alternated version this doesn't happen because the 
--initial exchange is part of the special behavior and in the opposite version it doesn't
--happen because we make sure one of them breaks the symmety. I can't make a working 
--version of this approach without going in one of those 2 directions, so it would just
--be redundant.

type LeftFork = !2Int;4+{First: FirstForkEnd, Second: SecondForkEnd}
type RightFork = !3Int;&1{First: FirstForkEnd, Second: SecondForkEnd}

FirstForkEnd = ?3();Close 5
SecondForkEnd = ?4();Close 6

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

-- lesserPhilosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof SecondFork 1->[3,6] ()
-- lesserPhilosopher id left right = 
--     let (_,left) = receive left in
--     let right = send () right in
--     sleep 500;
--     -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     close left;
--     wait right

-- greaterPhilosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof FirstForkTail 1->[3,6] ()
-- greaterPhilosopher id left right = 
--     let right = send () right in
--     let (_,left) = receive left in
--     sleep 500;
--     -- putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
--     wait right;
--     close left

philosopher : Int ->[top,bot] FirstFork ->[top,bot] dualof SecondFork 1->[1,6] ()
philosopher id left right = 
    sleep 500;
    let left = send id left in
    let (idr,right) = receive right in
    if(id < idr) 
    then
        let (_,left) = receive left in
        let right = send () right in
        sleep 500;
        close left;
        wait right
        -- lesserPhilosopher id left right
    else
        let right = send () right in
        let (_,left) = receive left in
        sleep 500;
        wait right;
        close left
        -- greaterPhilosopher id left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2;
    sleep 500
    -- print @String "Done!"
