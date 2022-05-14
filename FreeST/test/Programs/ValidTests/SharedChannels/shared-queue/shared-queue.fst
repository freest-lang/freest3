-- util 

-- | Receive a value from a shared channel
receiveUn : forall a . *?a -> a
receiveUn ch = fst[a, *?a] $ receive ch

-- | Send a value in a shared channel
sendUn : forall a . a -> *!a -> ()
sendUn x ch = let _ = send x ch in ()

-- | Fork a function n times
forkN : forall a . Int -> (() -> a) -> ()
forkN n f = runN[()] n (\_:() -> fork (f ()))

-- | Execute a function n times sequentially
runN : forall a . Int -> (() -> a) -> ()
runN n f =
    if n < 0
    then ()
    else 
        f ();
        runN[a] (n-1) f

-- |Launch n similar threads in parallel
parallel : forall a . Int -> (() -> a) -> ()
parallel n thread =
  if n <= 0
  then ()
  else fork $ thread ();
       parallel [a] (n - 1) thread

-- | Create a new child process and a channel through which it can communicate
-- with its parent process.
fork_ : forall a:SU . (dualof a -> ()) -> a
fork_ thread =
  let (x, y) = new a in
  fork $ thread y;
  x

-- channel types

type Request : SU = *+{Tail, Internal}

type Head : SU = {- Dequeue -} *?Int
type Tail : SU = {- Enqueue -} *!Int

type Internal : SL = ?Int; ?Internal

-- nodes

runHeadNode : Internal -o dualof Head -> ()
runHeadNode prev head = 
    let (i, prev) = receive prev in
    let (prev, _) = receive prev in

    sendUn [Int] i head;

    runHeadNode prev head

runTailNode : dualof Internal -o dualof Tail -> ()
runTailNode next tail =
    let i = receiveUn [Int] tail in
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
    (fork_ [Head] (runHeadNode internalC),
     fork_ [Tail] (runTailNode internalS))

enqueue : Int -> Queue -> ()
enqueue i queue = 
    sendUn [Int] i $ snd[Head, Tail] queue

dequeue : Queue -> Int
dequeue queue = 
    receiveUn[Int] $ fst[Head, Tail] queue

-- counter

type Counter : SU = *?Int

initCounter : Counter
initCounter = 
    let (counterC, counterS) = new Counter in
    fork $ runCounter 0 counterS;
    counterC

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    sendUn[Int] i counter;
    runCounter (i+1) counter

-- main

main : ()
main =
    let queue   = initQueue in
    let counter = initCounter in
    -- writer-reader concurrency, no writter-writer nor reader-reader concurrency
    forkN [()] 10 $ (\_:() -> enqueue (receiveUn[Int] counter) queue);
    runN [()]  10 $ (\_:() -> printIntLn (dequeue queue))
    -- writer-reader, writter-writer and reader-reader concurrency
    -- parallel [()] 10 $ (\_:() -> enqueue (receiveUn[Int] counter) queue);
    -- parallel [()]  10 $ (\_:() -> printIntLn (dequeue queue))
