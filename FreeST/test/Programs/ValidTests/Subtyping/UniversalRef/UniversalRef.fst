-- Here a general version is presented, using universal types.

-- With type operators, it would look something like
{-
type Ref        a = *?(RefService a)
type RefService a = +{Assign: !a, Deref: ?a};Close

type Source        a = *?(SourceService a)
type SourceService a = +{Deref : ?a};Close

type Sink        a = *?(SinkService a)
type SinkService a = +{Assign : !a};Close
-}

-- Constructor
ref : forall a:*T . a -> *?(+{Assign: !a, Deref: ?a};Close) 
ref n = forkWith @(*?(+{Assign: !a, Deref: ?a};Close)) 
                 @Diverge 
                 (runServer @(+{Assign: !a, Deref: ?a};Close) 
                            @a 
                            (refHandle @a)
                            n)

-- Manages state
refHandle : forall a:*T . a -> dualof (+{Assign: !a, Deref: ?a};Close) 1-> a 
refHandle v r  = match r with 
                 { Assign c -> let (v,c) = receive c in wait c; v
                 , Deref  c -> c |> send v |> wait; v
                 }

-- | Stores a value (:=).
-- Notice the type. A Ref can be safely downgraded to a Sink.
assign : forall a:*T . a -> *?(+{Assign: !a};Close) -> ()
assign v r = r |> receive_ @(+{Assign: !a};Close) 
               |> select Assign 
               |> send v 
               |> close

-- | Reads the stored value (!).
-- Notice the type. A Ref can be safely downgraded to a Source.
deref : forall a:*T . *?(+{Deref: ?a};Close)  -> a -- Source
deref r = let (v,c) = r |> receive_ @(+{Deref: ?a};Close)
                        |> select Deref
                        |> receive in 
          close c; v

-- Example
main : Bool 
main = let x = ref @Int 0 in
       let y = ref @Int 5 in
       let b = ref @Bool True in 
       let z = x     in
       assign @Int 2 x;
       assign @Int 1 z;
       (deref @Int x + deref @Int y); -- 6
       assign @Bool False b;
       deref @Bool b -- False
