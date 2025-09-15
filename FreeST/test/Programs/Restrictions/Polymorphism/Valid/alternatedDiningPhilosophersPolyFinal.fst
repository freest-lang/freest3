type Fork = forall p:(bot,top) => !p() ; ?p+1() ; Fork

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

philosopher : forall p:(bot,top), q:(p,top) => Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[p,q+1] ()
philosopher =
    forall p:(bot,top), q:(p,top) =>
    \id:Int ->
    \left: Fork ->
    \right: dualof Fork 1->
    sleep 500;
    if (even id) 
    then
        let left = send () (left{p}) in
        let (_,left) = receive left in
        let (_,right) = receive (right{q}) in
        let right = send () right in
        (philosopher{p+1}{q+1}) id left right
    else
        let (_,right) = receive (right{p}) in
        let right = send () right in
        let left = send () (left{q}) in
        let (_,left) = receive left in
        (philosopher{p+1}{q+1}) id left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork (\_:()1-> (philosopher{1}{3}) 1 fw1 fr3);
    fork (\_:()1-> (philosopher{1}{3}) 2 fw2 fr1);
    (philosopher{1}{3}) 3 fw3 fr2