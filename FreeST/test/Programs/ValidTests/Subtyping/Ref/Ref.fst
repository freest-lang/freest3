type T = Int

type Ref : *S = *?(RefService)
type RefService : *S = +{Assign: !T, Deref: ?T};End

-- A Ref can be downgraded to either a Source or a Sink
type Source : *S = *?(+{Deref : ?T};End)
type Sink   : *S = *?(+{Assign: !T};End)

-- Constructor
ref : T -> Ref 
ref n = let (r,s) = new Ref in
        fork (\_:() 1-> provider n s);
        r

-- Manages state and communication
provider : Int -> (dualof Ref) -> Diverge 
provider v r  = let (c,s) = new RefService in
                send c r;
                match s with {
                  Assign c -> let (v', c) = receive c in
                              close c;
                              provider v' r,
                  Deref  c -> c |> send v |> close;
                              provider v r
                }

-- Stores a value (:=)
assign : Int -> Sink -> ()
assign v r = let (svc, _) = receive r in 
             svc |> select Assign 
                 |> send v
                 |> close

-- Reads the stored value (!)
deref : Source -> T 
deref r = let (svc, _) = receive r in 
          let (n,c) = svc |> select Deref
                          |> receive in
          close c;  
          n 

-- Example
main : Int
main = let x = ref 0 in
       let y = ref 5 in
       let z = x     in
       assign 2 x;
       assign 1 z;
       deref x + deref y -- 6
