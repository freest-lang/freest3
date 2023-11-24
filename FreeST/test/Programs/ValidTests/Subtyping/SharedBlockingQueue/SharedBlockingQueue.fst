module SharedBlockingQueue where 

type T = Int 

type Queue        : *S = *?QueueSession
type QueueSession : 1S = +{Enqueue: !T, Dequeue: ?T};Close

-- A queue can be downgraded to a write-only queue
type WriteOnlyQueue : *S = *?EnqueueSession
type EnqueueSession : 1S = +{Enqueue : !T};Close

-- A queue can be downgraded to a read-only queue
type ReadOnlyQueue  : *S = *?DequeueSession
type DequeueSession : 1S = +{Dequeue : ?T};Close

-- In the subtype relation:
--
--   ReadOnlyQueue  WriteOnlyQueue
--               \  /
--              Queue

-- Inner workings 

-- Manage state and communications
queueHandle : (*!T, *?T) -> dualof QueueSession 1-> (*!T, *?T) 
queueHandle sr qs =
  let (s, r) = sr in 
  match qs with {
    Enqueue c -> fork (\_:() 1-> send (receiveAndWait @T c) s); sr,
    Dequeue c -> fork (\_:() 1-> send (receive_ @T r) c |> wait); sr
  }

-- Queue operations 

-- | Create an empty shared queue.
emptyQueue : Queue  
emptyQueue = forkWith @Queue @Diverge (runServer @QueueSession @(*!T, *?T) queueHandle (new @(*!T) ()))

-- | Add an element to the back of a queue. 
--   Write permission is enough.
enqueue : T -> WriteOnlyQueue -> ()
enqueue v q = q |> receive_ @EnqueueSession 
                |> select Enqueue  
                |> send v 
                |> close

-- | Remove an element from the front of a queue. 
--   If the queue is empty, block until an element is enqueued.
--   Read permission is enough.
dequeue : ReadOnlyQueue -> T 
dequeue q = q |> receive_ @DequeueSession 
              |> select Dequeue
              |> receiveAndClose @T 

main : ()
main = let q = emptyQueue in 
       let (w,r) = new @(*!Int) () in 
       let n = 10 in 
       parallel @() n (\_:() -> enqueue 1 q); -- lin/un function subtyping
       parallel @() n (\_:() -> send (dequeue q) w; ());
       repeat   @() n (\_:() -> print @Int $ receive_ @Int r)
