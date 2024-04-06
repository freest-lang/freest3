-- channel types

type Head : *S = {- Dequeue -} *?Int
type Tail : *S = {- Enqueue -} *!Int

type Internal : 1S = ?Int; ?Internal; Wait 

-- nodes

runHeadNode : Internal -> dualof Head 1-> ()
runHeadNode prev head = 
    let (i, prev) = receive prev in
    send_ @Int i head;
    runHeadNode (receiveAndWait @Internal prev) head

runTailNode : dualof Internal -> dualof Tail 1-> ()
runTailNode next tail =
    let i = receive_ @Int tail in
    let (prev', next') = new @Internal () in
    fork (\_:()1-> send i next |> send prev' |> close);
    runTailNode next' tail
    -- Internal error at Validation.Rename.rename: dualof
    -- runTailNode (fork_ [Internal] (λ c:dualof Internal -> send c (send i next))) tail

-- queue

type Queue = (Head, Tail)

initQueue : Queue
initQueue =
    let (internalC, internalS) = new @Internal () in
    (forkWith @Head @() (runHeadNode internalC),
     forkWith @Tail @() (runTailNode internalS))

enqueue : Int -> Queue -> ()
enqueue i queue = 
    send_ @Int i $ snd @Head @Tail queue

dequeue : Queue -> Int
dequeue queue = 
    receive_@Int $ fst @Head @Tail queue

-- counter

type Counter : *S = *?Int

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    send_ @Int i counter;
    runCounter (i+1) counter

initCounter : Counter
initCounter = 
    let (counterC, counterS) = new @Counter () in
    fork (\_:() 1-> runCounter 0 counterS);
    counterC

-- main

main : ()
main =
    let queue   = initQueue in
    let counter = initCounter in
    -- writer-reader concurrency, no writter-writer nor reader-reader concurrency
    parallel @() 10 $ (\_:() -> enqueue (receive_ @Int counter) queue);
    repeat @()  10 $ (\_:() -> print @Int (dequeue queue))
    -- writer-reader, writter-writer and reader-reader concurrency
    -- parallel [()] 10 $ (\_:() -> enqueue (receiveUn[Int] counter) queue);
    -- parallel [()]  10 $ (\_:() -> printIntLn (dequeue queue))
