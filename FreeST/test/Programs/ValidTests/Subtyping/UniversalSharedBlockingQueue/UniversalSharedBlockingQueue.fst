module UniversalSharedBlockingQueue where 

{-

type Queue        = *?QueueService
type QueueService = +{Enqueue: !T, Dequeue: ?T};Close

-- A queue can be downgraded to a write-only queue
type WriteOnlyQueue = *?EnqueueService
type EnqueueService = +{Enqueue : !T};Close

-- A queue can be downgraded to a read-only queue
type ReadOnlyQueue  = *?DequeueService
type DequeueService = +{Dequeue : ?T};Close

-- In the subtype relation:
--
--   ReadOnlyQueue  WriteOnlyQueue
--               \  /
--              Queue

-}

-- Inner workings 

-- Manage state and communications
queueHandle : (*!a, *?a) -> &{Enqueue: ?a, Dequeue: !a};Wait 1-> (*!a, *?a) 
queueHandle sr qs =
  let (s, r) = sr in 
  match qs with {
    Enqueue c -> fork (\_:() 1-> send (receiveAndWait @a c) s); sr,
    Dequeue c -> fork (\_:() 1-> send (receive_ @a r) c |> wait); sr
  }

-- Queue operations 

-- | Create an empty shared queue.
emptyQueue : () -> *?(+{Enqueue: !a, Dequeue: ?a};Close)  
emptyQueue _ = forkWith @(*?(+{Enqueue: !a, Dequeue: ?a};Close)) 
                        @Diverge 
                        (runServer @(+{Enqueue: !a, Dequeue: ?a};Close) 
                                   @(*!a, *?a) 
                                   (queueHandle @a) 
                                   (new @(*!a) ()))

-- | Add an element to the back of a queue. 
--   Write permission is enough.
enqueue : a -> *?(+{Enqueue: !a};Close) -> ()
enqueue v q = q |> receive_ @(+{Enqueue: !a};Close) 
                |> select Enqueue  
                |> send v 
                |> close

-- | Remove an element from the front of a queue. 
--   If the queue is empty, block until an element is enqueued.
--   Read permission is enough.
dequeue : *?(+{Dequeue: ?a};Close) -> a
dequeue q = q |> receive_ @(+{Dequeue: ?a};Close) 
              |> select Dequeue
              |> receiveAndClose @a

main : ()
main = let q = emptyQueue @Bool () in 
       let (w,r) = new @(*!Bool) () in 
       let n = 10 in 
       parallel @() n (\_:() -> enqueue @Bool True q); -- lin/un function subtyping
       parallel @() n (\_:() -> send (dequeue @Bool q) w; ());
       repeat   @() n (\_:() -> print @Bool $ receive_ @Bool r)
