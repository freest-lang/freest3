type T = Int

type Ref : *S = *?(RefService)
type RefService : *S = +{Assign: !T, Deref: ?T};End

-- A Ref can be downgraded to either a Source or a Sink
type SourceService : *S = +{Deref : ?T};End
type Source : *S = *?(SourceService)

type SinkService : *S = +{Assign : !T};End
type Sink   : *S = *?(+{Assign: !T};End)

-- Constructor
ref : T -> Ref 
ref n = forkWith @Ref @Diverge (runServer @RefService @T refHandle n)

-- Manages state and communication
refHandle : T -> dualof RefService 1-> T 
refHandle v r  = match r with {
                  Assign c -> let (v,c) = receive c in close c; v,
                  Deref  c -> c |> send v |> close; v
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
