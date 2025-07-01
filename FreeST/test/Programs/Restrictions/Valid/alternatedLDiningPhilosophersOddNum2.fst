type Fork1 = !a();?b();Close c
type Fork2 = !d();?e();Close f
type Fork3 = !g();?h();Close i

philosopher1 : Int ->[top,bot] Fork1 ->[top,bot] dualof Fork3 1->[a,c] ()
philosopher1 id left right = 
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    wait right;
    close left

philosopher2 : Int ->[top,bot] Fork2 ->[top,bot] dualof Fork1 1->[d,c] ()
philosopher2 id left right = 
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    close left;
    wait right

philosopher3 : Int ->[top,bot] Fork3 ->[top,bot] dualof Fork2 1->[g,i] ()
philosopher3 id left right =
    let (_,right) = receive right in
    let right = send () right in
    let left = send () left in
    let (_,left) = receive left in
    wait right;
    close left

philosopher4 : Int ->[top,bot] Fork2 ->[top,bot] dualof Fork3 1->[d,i] ()
philosopher4 id left right =
    let left = send () left in
    let (_,left) = receive left in
    let (_,right) = receive right in
    let right = send () right in
    close left;
    wait right

main : ()
main = 
    let (fw1, fr1) = new @Fork1 () in
    let (fw2, fr2) = new @Fork2 () in
    let (fw3, fr3) = new @Fork3 () in
    let (fw4, fr4) = new @Fork2 () in
    let (fw5, fr5) = new @Fork3 () in
    fork (\_:()1-> philosopher1 1 fw1 fr5);
    fork (\_:()1-> philosopher2 2 fw2 fr1);
    fork (\_:()1-> philosopher3 3 fw3 fr2);
    fork (\_:()1-> philosopher4 4 fw4 fr3);
    philosopher3 5 fw5 fr4
