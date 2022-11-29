module UniversalSharedBlockingQueue where 

{-

type Queue        : *S = *?QueueService
type QueueService : 1S = +{Enqueue: !T, Dequeue: ?T};End

-- A queue can be downgraded to a write-only queue
type WriteOnlyQueue : *S = *?EnqueueService
type EnqueueService : 1S = +{Enqueue : !T};End

-- A queue can be downgraded to a read-only queue
type ReadOnlyQueue  : *S = *?DequeueService
type DequeueService : 1S = +{Dequeue : ?T};End

-- In the subtype relation:
--
--   ReadOnlyQueue  WriteOnlyQueue
--               \  /
--              Queue

-}

-- Inner workings 

-- Manage state and communications
queueHandle : forall a:*T . (*!a, *?a) -> &{Enqueue: ?a, Dequeue: !a};End 1-> (*!a, *?a) 
queueHandle sr qs =
  let (s, r) = sr in 
  match qs with {
    Enqueue c -> fork (\_:() 1-> send (receiveAndClose @a c) s); sr,
    Dequeue c -> fork (\_:() 1-> send (receive_ @a r) c |> close); sr
  }

-- Queue operations 

-- | Create an empty shared queue.
emptyQueue : forall a:*T . () -> *?(+{Enqueue: !a, Dequeue: ?a};End)  
emptyQueue _ = forkWith @(*?(+{Enqueue: !a, Dequeue: ?a};End)) 
                        @Diverge 
                        (runServer @(+{Enqueue: !a, Dequeue: ?a};End) 
                                   @(*!a, *?a) 
                                   (queueHandle @a) 
                                   (new *!a))

-- | Add an element to the back of a queue. 
--   Write permission is enough.
enqueue : forall a:*T . a -> *?(+{Enqueue: !a};End) -> ()
enqueue v q = q |> receive_ @(+{Enqueue: !a};End) 
                |> select Enqueue  
                |> send v 
                |> close

-- | Remove an element from the front of a queue. 
--   If the queue is empty, block until an element is enqueued.
--   Read permission is enough.
dequeue : forall a:*T . *?(+{Dequeue: ?a};End) -> a
dequeue q = q |> receive_ @(+{Dequeue: ?a};End) 
              |> select Dequeue
              |> receiveAndClose @a

main : ()
main = let q = emptyQueue @Bool () in 
       let (w,r) = new *!Bool in 
       let n = 10 in 
       parallel @() n (\_:() -> enqueue @Bool True q); -- lin/un function subtyping
       parallel @() n (\_:() -> send (dequeue @Bool q) w; ());
       repeat   @() n (\_:() -> printBoolLn $ receive_ @Bool r)
