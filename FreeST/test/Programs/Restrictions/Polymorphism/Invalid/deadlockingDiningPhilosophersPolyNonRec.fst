type Fork = forall p:(bot,top) => !p() ; ?p+2(); Close p+4

philosopher : forall p:(bot,top), q:(p,top) => Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[p,q+4] ()
philosopher =
    forall p:(bot,top), q:(p,top) =>
    \id:Int ->
    \left: Fork ->
    \right: dualof Fork 1->
    let left = send () (left{p}) in
    let (_,right) = receive (right{q}) in
    let (_,left) = receive left in
    let right = send () right in
    close left;
    wait right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork (\_:()1-> (philosopher{1}{3}) 1 fw1 fr3);
    fork (\_:()1-> (philosopher{2}{1}) 2 fw2 fr1);
    (philosopher{3}{2}) 3 fw3 fr2