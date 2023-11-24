f : (&{A: Skip, B: Skip, C:Skip};Wait, +{A: Skip, B: Skip};Close) -> ()
f p = let (b,c) = p in
      fork (\_:() 1-> c |> select A |> close);
      match b with {
        A c -> wait c,
        B c -> wait c
      }

main : ()
main = f $ new @(&{A: Skip, B: Skip};Wait) ()