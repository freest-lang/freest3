type Fork = !p();?p+1();Close p+2

philosopher : Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[p,p+2] ()
philosopher id left right = 
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    close left;
    wait right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork (\_:()1-> philosopher 1 fw1 fr3);
    fork (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2