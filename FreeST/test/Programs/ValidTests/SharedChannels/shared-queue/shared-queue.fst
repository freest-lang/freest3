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

-- channel types

type Request : SU = *+{Tail, Internal}

type Head : SU = {- Dequeue -} *?Int
type Tail : SU = {- Enqueue -} *!Int

type Internal : SL = ?Int; ?Internal

-- nodes

runHeadNode : dualof Head -> Internal -> ()
runHeadNode head prev = 
    let (i, prev) = receive prev in
    let (prev, _) = receive prev in

    sendUn[Int] i head;

    runHeadNode head prev

runTailNode : dualof Tail -> dualof Internal -> ()
runTailNode tail next =
    let i = receiveUn[Int] tail in
    let (prev', next') = new Internal in

    fork $ send prev' (send i next);

    runTailNode tail next'

-- queue

type Queue = (Head, Tail)

initQueue : Queue
initQueue =
    let (internalC, internalS) = new Internal in

    let (headC, headS) = new Head in
    fork $ runHeadNode headS internalC;

    let (tailC, tailS) = new Tail in
    fork $ runTailNode tailS internalS;

    (headC, tailC)

enqueue : Int -> Queue -> ()
enqueue i queue = 
    let _ = send i $ snd[Head, Tail] queue in ()

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
    
    forkN[()] 10 $ (\_:() -> enqueue (receiveUn[Int] counter) queue);
    runN[()]  10 $ (\_:() -> printIntLn (dequeue queue))