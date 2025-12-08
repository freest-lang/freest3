type Worker = forall a : (bot,top) => +a{Start: ?a+2()};Worker
type Sched : 1S = forall b : (bot,top) => +b{Start: +b+4{Next: Sched}}

follower : forall p:(bot,top) => dualof Sched ->[top,bot] Worker 1->[p,bot] Sched 1->[p,next+4] ()
follower = 
    forall p:(bot,top) =>
    \prev: dualof Sched ->
    \worker: Worker 1->
    \next: Sched 1->
    match (inst prev) with {
        Start prev -> 
            let worker = select Start (inst worker) in
            let next = select Start (inst next) in
            let (_, worker) = receive worker in
            match prev with {
                Next prev ->
                    let next = select Next next in
                    (follower {priority prev}) prev worker next
            }
    }

leader : forall p:(bot,top) => Worker ->[top,bot] dualof Sched 1->[p,bot] Sched 1->[p,next+4] ()
leader = 
    forall p:(bot,top) =>
    \worker: Worker ->
    \prev: dualof Sched 1->
    \next: Sched 1->
    let worker = select Start (inst worker) in
    let next = select Start (inst next) in
    let (_, worker) = receive worker in
    let next = select Next next in
    (follower {priority worker}) prev worker next

worker : forall p:(bot,top) => dualof Worker ->[top,x+2] ()
worker = 
    forall p:(bot,top) =>
    \x: dualof Worker ->
    match (inst x) with {
        Start x ->        
            let x = send () x in
            (worker {priority x}) x
    }

main : ()
main =
    let (a1, b1) = new @Worker {1,12} () in 
    let (a2, b2) = new @Worker {3,12} () in 
    let (a3, b3) = new @Worker {5,12} () in 
    let (c1, d1) = new @Sched {2,12} () in 
    let (c2, d2) = new @Sched {4,12} () in 
    let (c3, d3) = new @Sched {6,12} () in 

    fork (\_:()1-> (leader {priority a1}) a1 d3 c1);
    fork (\_:()1-> (follower {priority d1}) d1 a2 c2);
    fork (\_:()1-> (follower {priority d2}) d2 a3 c3);

    fork (\_:()1-> (worker {priority b1}) b1);
    fork (\_:()1-> (worker {priority b2}) b2);
    (worker {priority b3}) b3;
    ()
