type Fork1 = !a();?b();Close c
type Fork2 = !d();?e();Close f
type Fork3 = !g();?h();Close i

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

regularPhilosopher1 : Int ->[top,bot] Fork1 ->[top,bot] dualof Fork3 1->[a,i] ()
regularPhilosopher1 id left right = 
    sleep 500;
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    close left;
    wait right

regularPhilosopher2 : Int ->[top,bot] Fork2 ->[top,bot] dualof Fork1 1->[d,c] ()
regularPhilosopher2 id left right = 
    sleep 500;
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    sleep 500;
    close left;
    wait right

oppositePhilosopher : Int ->[top,bot] Fork3 ->[top,bot] dualof Fork2 1->[g,i] ()
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
