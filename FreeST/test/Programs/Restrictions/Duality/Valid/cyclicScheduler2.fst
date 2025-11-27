type Worker = forall a : (bot,top) => +a{Start: ?a+2()};Worker
type Sched : 1S = forall b : (bot,top) => +b{Start: +b+4{Next: Sched}} -- troquei este tipo pq acho que Next tem que se seguir a Start e da forma como estava, podiamos escolher qq um em primeiro lugar

follower : forall p:(bot,top) => dualof Sched ->[top,bot] Worker 1->[b,bot] Sched 1->[b,next+4] ()
follower = 
    forall p:(bot,top) =>
    \prev: dualof Sched ->
    \worker: Worker 1->
    \next: Sched 1->
    match (inst prev) with {                                    --1
        Start prev -> 
            let worker = select Start (inst worker) in          --2
            let next = select Start (inst next) in              --3
            let (_, worker) = receive worker in                 --4
            match prev with {                                   --5
                Next prev ->                                    -- removi alguns (inst next) que estavam a mais
                    let next = select Next next in              --7 segundo o tipo, temos prioridade 7 (segundo o programa podia ser 6, mas assim ficamos apenas com um tipo, em vez de ter que definir mais um que so muda nesta prioridade especifica)
                    (follower {priority prev}) prev worker next   -- prev é o que está no contexto e é o que tem menor prioridade
            }
    }

--p,w,n,w,p,n

leader : forall p:(bot,top) => Worker ->[top,bot] dualof Sched 1->[a,bot] Sched 1->[a,next+4] () -- troquei worker e prev pq o worker é o que tem menor prioridade. assim nao precisamos de 2 binders
leader = 
    forall p:(bot,top) =>
    \worker: Worker ->
    \prev: dualof Sched 1->
    \next: Sched 1->
    let worker = select Start (inst worker) in                  --1
    let next = select Start (inst next) in                      --2 
    let (_, worker) = receive worker in                         --3
    let next = select Next next in                              --6
    (follower {priority worker}) prev worker next                   -- acho que aqui o lider comporta-se como um follower, não precisa de receber start e next de seguida nunca mais. por isso retirei tb o int de input

--w,n,w,n

worker : forall p:(bot,top) => dualof Worker ->[top,x+2] ()
worker = 
    forall p:(bot,top) =>
    \x: dualof Worker ->
    match (inst x) with { -- 1
        Start x ->        
            let x = send () x in -- 3 
            (worker {priority x}) x
    }

main : ()
main = -- fiz a versão com 3 sched e 3 workers
    let (a1, b1) = new @Worker {1,7} () in 
    let (a2, b2) = new @Worker {3,7} () in 
    let (a3, b3) = new @Worker {5,7} () in 
    let (c1, d1) = new @Sched {2,7} () in 
    let (c2, d2) = new @Sched {4,7} () in 
    let (c3, d3) = new @Sched {6,7} () in 

    fork (\_:()1-> (leader {priority a1}) a1 d3 c1); --A1
    fork (\_:()1-> (follower {priority d1}) d1 a2 c2); --A2
    fork (\_:()1-> (follower {priority d2}) d2 a3 c3); --A3

    fork (\_:()1-> (worker {priority b1}) b1); --P1
    fork (\_:()1-> (worker {priority b2}) b2); --P2
    (worker {priority b3}) b3;
    ()
