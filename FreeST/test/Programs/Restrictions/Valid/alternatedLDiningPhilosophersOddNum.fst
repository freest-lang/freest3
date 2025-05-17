type Fork1 = !5();?6();Close 9
type Fork2 = !1();?2();Close 7
type Fork3 = !3();?4();Close 8

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher1 : Int ->[3,bot] Fork1 ->[3,bot] dualof Fork3 1->[3,9] ()
philosopher1 id left right = 
    sleep 500;
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    sleep 500;
    wait right;
    close left

philosopher2 : Int ->[1,bot] Fork2 ->[1,bot] dualof Fork1 1->[5,9] ()
philosopher2 id left right = 
    sleep 500;
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    sleep 500;
    close left;
    wait right

philosopher3 : Int ->[1,bot] Fork3 ->[1,bot] dualof Fork2 1->[1,8] ()
philosopher3 id left right =
    sleep 500;
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    sleep 500;
    wait right;
    close left

main : ()
main = 
    let (fw1, fr1) = new @Fork1 () in
    let (fw2, fr2) = new @Fork2 () in
    let (fw3, fr3) = new @Fork3 () in
    fork @() (\_:()1-> philosopher1 1 fw1 fr3);
    fork @() (\_:()1-> philosopher2 2 fw2 fr1);
    philosopher3 3 fw3 fr2;
    sleep 500
