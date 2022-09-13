-- | generic queue

-- Head is dequeue or peek
runHead : forall a . (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) -> dualof *?(+{Dequeue: ?a, Peek: ?a}) 1-> Diverge
runHead prev ch =
    let ch' = accept_ @+{Dequeue: ?a, Peek: ?a} ch in
    match ch' with {
        Dequeue ch' ->
            -- dequeue prev node and receive channel to next
            let (i    , prev) = select Dequeue prev |> receive in
            let (prev', prev) = receive prev in
            close prev;
            -- send value to client
            sink @Skip $ send i ch';
            -- recur
            runHead @a prev' ch,
        Peek ch' -> 
            -- delegate peek to value process
            let prev = select Peek prev |> send ch' in
            -- recur
            runHead @a prev ch
    }

-- Tail is enqueue
runTail : forall a . dualof (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) -> dualof *!a 1-> Diverge
runTail next tail =
    let i = receive_ @a tail in
    let next' = forkWith @dualof (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) @() (runValue @a i next) in
    runTail @a next' tail

-- Value nodes
runValue : forall a . a -> dualof (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) -> (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) 1-> ()
runValue i next prev =
    match next with {
        Dequeue next ->
            send i next
            |> send prev
            |> close,
        Peek next ->
            let (c, next) = receive next in
            fork $ (\_:() 1-> sink @Skip (send i c)); -- parallel reads happen here
            runValue @a i next prev
    }

--

initQueue : forall a . () -> (*!a, *?(+{Dequeue: ?a, Peek: ?a}))
initQueue _ =
    let (internalC, internalS) = new (rec x:1S . +{Dequeue: ?a; ?x; End, Peek: !(!a); x}) in
    ( forkWith @*!a                          @() (runTail @a internalS)
    , forkWith @*?(+{Dequeue: ?a, Peek: ?a}) @() (runHead @a internalC)
    )

enqueue : forall a . a -> (*!a, *?(+{Dequeue: ?a, Peek: ?a})) -> ()
enqueue i queue = 
    fst @*!a @*?(+{Dequeue: ?a, Peek: ?a}) queue 
    |> send_ @a i

dequeue : forall a . (*!a, *?(+{Dequeue: ?a, Peek: ?a})) -> a
dequeue queue = 
    snd @*!a @*?(+{Dequeue: ?a, Peek: ?a}) queue
    |> receive_ @+{Dequeue: ?a, Peek: ?a}
    |> select Dequeue
    |> receive
    |> fst @a @Skip

peek : forall a . (*!a, *?(+{Dequeue: ?a, Peek: ?a})) -> a
peek queue = 
    snd @*!a @*?(+{Dequeue: ?a, Peek: ?a}) queue
    |> receive_ @+{Dequeue: ?a, Peek: ?a}
    |> select Peek
    |> receive
    |> fst @a @Skip

-- 

type IntQueue = (*!Int, *?(+{Dequeue: ?Int, Peek: ?Int}))

main : ()
main = 
    let queue = initQueue @Int () in
    fork $ enqueueClient 1    queue;
    fork $ dequeueClient      queue;
    fork $ peekClient         queue;
    fork $ peekClient         queue;
    fork $ enqueueClient 2    queue;
    fork $ peekClient         queue;
    fork $ enqueueClient 3    queue;
    fork $ peekClient         queue;
    fork $ dequeueClient      queue;
    fork $ peekClient         queue;
    fork $ enqueueClient (-1) queue;
    fork $ dequeueClient      queue;
    diverge
    

enqueueClient : Int -> IntQueue -> () -> ()
enqueueClient i queue _ =
    enqueue @Int i queue; putStrLn ("enqueue > " ++ (show @Int i))

dequeueClient : IntQueue -> () -> ()
dequeueClient queue _ = 
    let i = dequeue @Int queue in
    putStrLn ("dequeue > " ++ (show @Int i))

peekClient : IntQueue -> () -> ()
peekClient queue _ = 
    let i = peek @Int queue in
    putStrLn ("peek    > " ++ (show @Int i))


