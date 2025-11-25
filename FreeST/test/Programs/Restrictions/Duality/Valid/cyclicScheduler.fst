type Worker = forall a : (bot,top) => +a{Start: ?a+2(); Worker}
type Sched = forall b : (bot,top) => &b{Start: Sched, Next: Sched}

follower : forall p:(bot,top) => Sched ->[top,bot] Worker 1->[b,bot] dualof Sched 1->[b,next] ()
follower = 
    forall p:(bot,top) =>
    \prev: Sched ->
    \worker: Worker 1->
    \next: dualof Sched 1->
    match (inst prev) with {                                    --1
        Start prev -> 
            let worker = select Start (inst worker) in          --2
            let next = select Start (inst next) in              --3
            let (_, worker) = receive worker in                 --4
            match (inst prev) with {                            --5
                Next prev ->
                    let next = select Next (inst next) in       --6
                    follower {priority next} prev worker next         --p,w,n,w,p,n --- 1,2,3,4(+2),5(inst +4),6(inst +3)
            }
    }

leader : forall p:(bot,top) => Sched ->[top,bot] Worker 1->[b,bot] dualof Sched 1->[b,bot] Int 1->[b,next] ()
leader = 
    forall p:(bot,top) =>
    \prev: Sched ->
    \worker: Worker 1->
    \next: dualof Sched 1->
    \i: Int 1->
    let worker = select Start (inst worker) in                  --1
    let next = select Start (inst next) in                      --2 
    let (_, worker) = receive worker in                         --3
    let next = select Next (inst next) in                       --4
    match (inst prev) with {                                    --5
        Start prev ->
            match (inst prev) with {                            --6
                Next prev ->
                    leader {priority next} prev worker next (i + 1)   --w,n,w,n,p,p --- 1,2,3(+2),4(inst +2),5,6(inst +1)
            }
    }

worker : forall p:(bot,top) => dualof Worker ->[top,x+1] ()
worker = 
    forall p:(bot,top) =>
    \x: dualof Worker ->
    match (inst x) with {
        Start x -> 
            let x = send () x in 
            worker {priority x} x
    }

main : ()
main =
    let (a1, b1) = new @Worker {13,14} () in
    let (a2, b2) = new @Worker {15,16} () in
    let (a3, b3) = new @Worker {17,18} () in
    let (a4, b4) = new @Worker {19,20} () in
    let (a5, b5) = new @Worker {21,22} () in
    let (a6, b6) = new @Worker {23,24} () in
    let (c1, d1) = new @Sched {1,2} () in
    let (c2, d2) = new @Sched {3,4} () in
    let (c3, d3) = new @Sched {5,6} () in
    let (c4, d4) = new @Sched {7,8} () in
    let (c5, d5) = new @Sched {9,10} () in
    let (c6, d6) = new @Sched {11,12} () in

    fork (\_:()1-> leader {priority d1} c6 a1 d1 0); --A1
    fork (\_:()1-> follower {priority c1} c1 a2 d2); --A2
    fork (\_:()1-> follower {priority c2} c2 a3 d3); --A3
    fork (\_:()1-> follower {priority c3} c3 a4 d4); --A4
    fork (\_:()1-> follower {priority c4} c4 a5 d5); --A5
    fork (\_:()1-> follower {priority c5} c5 a6 d6); --A6

    fork (\_:()1-> worker {priority b1} b1); --P1
    fork (\_:()1-> worker {priority b2} b2); --P2
    fork (\_:()1-> worker {priority b3} b3); --P3
    fork (\_:()1-> worker {priority b4} b4); --P4
    fork (\_:()1-> worker {priority b5} b5); --P5
    worker {priority b6} b6;
    ()