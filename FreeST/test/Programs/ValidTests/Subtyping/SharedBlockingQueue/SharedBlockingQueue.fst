module SharedBlockingQueue where 

type T = Int 

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

-- Inner workings 

-- Manage state and communications
queueHandle : (*!T, *?T) -> dualof QueueService 1-> (*!T, *?T) 
queueHandle sr qs =
  let (s, r) = sr in 
  match qs with {
    Enqueue c -> fork (\_:() 1-> send (receiveAndClose @T c) s); sr,
    Dequeue c -> fork (\_:() 1-> send (receive_ @T r) c |> close); sr
  }

-- Queue operations 

-- | Create an empty shared queue.
emptyQueue : Queue  
emptyQueue = forkWith @Queue @Diverge (runServer @QueueService @(*!T, *?T) queueHandle (new *!T))

-- | Add an element to the back of a queue. 
--   Write permission is enough.
enqueue : T -> WriteOnlyQueue -> ()
enqueue v q = q |> receive_ @EnqueueService 
                |> select Enqueue  
                |> send v 
                |> close

-- | Remove an element from the front of a queue. 
--   If the queue is empty, block until an element is enqueued.
--   Read permission is enough.
dequeue : ReadOnlyQueue -> T 
dequeue q = q |> receive_ @DequeueService 
              |> select Dequeue
              |> receiveAndClose @T 

main : ()
main = let q = emptyQueue in 
       let (w,r) = new *!Int in 
       let n = 10 in 
       parallel @() n (\_:() -> enqueue 1 q); -- lin/un function subtyping
       parallel @() n (\_:() -> send (dequeue q) w; ());
       repeat   @() n (\_:() -> printIntLn $ receive_ @Int r)
