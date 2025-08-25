type Fork = forall p:(bot,top) => !p() ; ?p+1() ; Fork
-- type Fork = forall p:(bot,top) => !p() ; ?p+1() ; !p+4() ; Fork

sleep : Int ->[top,bot] ()
sleep n = if n == 0 then () else sleep (n-1)

evenPhilosopher : forall p:(bot,top), q:(p,top) => Fork ->[top,bot] dualof Fork 1->[p,q+1] ()
evenPhilosopher =
    forall p:(bot,top), q:(p,top) =>
    \left: Fork ->
    \right: dualof Fork 1->
    let left = send () (left{p}) in
    let (_,left) = receive left in
    let (_,right) = receive (right{q}) in
    let right = send () right in
    -- sleep 500;
    -- let left = send () left in
    -- let (_,right) = receive right in
    evenPhilosopher{p+1}{q+1} left right

oddPhilosopher : forall p:(q,top), q:(bot,top) => Fork ->[top,bot] dualof Fork 1->[p,p+1] ()
oddPhilosopher =
    forall p:(q,top), q:(bot,top) =>
    \left: Fork ->
    \right: dualof Fork 1->
    let (_,right) = receive (right{q}) in
    let right = send () right in
    let left = send () (left{p}) in
    let (_,left) = receive left in
    -- sleep 500;
    -- let (_,right) = receive right in
    -- let left = send () left in
    oddPhilosopher{p+1}{q+1} left right

philosopher : Int ->[top,bot] Fork ->[top,bot] dualof Fork 1->[p,bot] ()
philosopher =
    \id:Int ->
    \left: Fork ->
    \right: dualof Fork 1->
    sleep 500;
    if (even id) 
    then
        (evenPhilosopher{1}{3}) left right
    else
        (oddPhilosopher{3}{1}) left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork (\_:()1-> philosopher 1 fw1 fr3);
    fork (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2

--the problem here is that conditional branches with different
--orders are impossible