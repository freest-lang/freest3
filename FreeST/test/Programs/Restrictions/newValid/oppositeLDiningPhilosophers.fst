type Fork1 = !2();?5();Close 8
type Fork2 = !1();?4();Close 7
type Fork3 = !3();?6();Close 9

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

regularPhilosopher1 : Int ->[2,bot] Fork1 ->[2,bot] dualof Fork3 1->[3,9] ()
regularPhilosopher1 id left right = 
    sleep 500;
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    close left;
    wait right

regularPhilosopher2 : Int ->[1,bot] Fork2 ->[1,bot] dualof Fork1 1->[2,8] ()
regularPhilosopher2 id left right = 
    sleep 500;
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    close left;
    wait right

oppositePhilosopher : Int ->[1,bot] Fork3 ->[1,bot] dualof Fork2 1->[1,9] ()
oppositePhilosopher id left right =
    sleep 500;
    let (_,right) = receive right in
    let left = send () left in
    let right = send () right in
    let (_,left) = receive left in
    sleep 500;
    wait right;
    close left

main : ()
main = 
    let (fw1, fr1) = new @Fork1 () in
    let (fw2, fr2) = new @Fork2 () in
    let (fw3, fr3) = new @Fork3 () in
    fork @() (\_:()1-> regularPhilosopher1 1 fw1 fr3);
    fork @() (\_:()1-> regularPhilosopher2 2 fw2 fr1);
    oppositePhilosopher 3 fw3 fr2;
    sleep 500
    -- print @String "Done!"
