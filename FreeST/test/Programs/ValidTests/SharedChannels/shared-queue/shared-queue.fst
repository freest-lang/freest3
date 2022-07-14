type Diverge = ()

-- channel types

type Head : SU = {- Dequeue -} *?Int
type Tail : SU = {- Enqueue -} *!Int

type Internal : SL = ?Int; ?Internal

-- nodes

runHeadNode : Internal -> dualof Head -o ()
runHeadNode prev head = 
    let (i, prev) = receive prev in
    let (prev, _) = receive prev in

    send_ [Int] i head;

    runHeadNode prev head

runTailNode : dualof Internal -> dualof Tail -o ()
runTailNode next tail =
    let i = receive_ [Int] tail in
    let (prev', next') = new Internal in
    fork $ send prev' (send i next);
    runTailNode next' tail
    -- Internal error at Validation.Rename.rename: dualof
    -- runTailNode (fork_ [Internal] (λ c:dualof Internal -> send c (send i next))) tail

-- queue

type Queue = (Head, Tail)

initQueue : Queue
initQueue =
    let (internalC, internalS) = new Internal in
    (forkWith [Head] (runHeadNode internalC),
     forkWith [Tail] (runTailNode internalS))

enqueue : Int -> Queue -> ()
enqueue i queue = 
    send_ [Int] i $ snd[Head, Tail] queue

dequeue : Queue -> Int
dequeue queue = 
    receive_[Int] $ fst[Head, Tail] queue

-- counter

type Counter : SU = *?Int

initCounter : Counter
initCounter = 
    let (counterC, counterS) = new Counter in
    fork $ runCounter 0 counterS;
    counterC

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    send_ [Int] i counter;
    runCounter (i+1) counter

-- main

main : ()
main =
    let queue   = initQueue in
    let counter = initCounter in
    -- writer-reader concurrency, no writter-writer nor reader-reader concurrency
    parallel 10 $ (\_:() -> enqueue (receive_ [Int] counter) queue);
    repeat [()]  10 $ (\_:() -> printIntLn (dequeue queue))
    -- writer-reader, writter-writer and reader-reader concurrency
    -- parallel [()] 10 $ (\_:() -> enqueue (receiveUn[Int] counter) queue);
    -- parallel [()]  10 $ (\_:() -> printIntLn (dequeue queue))
