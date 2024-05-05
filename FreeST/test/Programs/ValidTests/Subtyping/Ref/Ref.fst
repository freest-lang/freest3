type T = Int

type Ref = *?(RefService)
type RefService = +{Assign: !T, Deref: ?T};Close

-- A Ref can be downgraded to either a Source or a Sink
type SourceService = +{Deref : ?T};Close
type Source = *?(SourceService)

type SinkService = +{Assign : !T};Close
type Sink = *?(+{Assign: !T};Close)

-- Constructor
ref : T -> Ref 
ref n = forkWith @Ref @Diverge (runServer @RefService @T refHandle n)

-- Manages state and communication
refHandle : T -> dualof RefService 1-> T 
refHandle v r  = match r with {
                  Assign c -> let (v,c) = receive c in wait c; v,
                  Deref  c -> c |> send v |> wait; v
                }

-- | Stores a value (:=)
-- Notice the type. A Ref can be safely downgraded to a Sink.
assign : Int -> Sink -> ()
assign v r = r |> receive_ @SinkService 
               |> select Assign 
               |> send v 
               |> close

-- | Reads the stored value (!)
-- Notice the type. A Ref can be safely downgraded to a Source.
deref : Source -> T 
deref r = let (v,c) = r |> receive_ @SourceService 
                        |> select Deref
                        |> receive in
          close c; v
            

-- Example
main : Int
main = let x = ref 0 in
       let y = ref 5 in
       let z = x     in
       assign 2 x;
       assign 1 z;
       deref x + deref y -- 6
