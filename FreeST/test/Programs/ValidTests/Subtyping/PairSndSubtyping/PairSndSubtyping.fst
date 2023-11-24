f : (+{A: Skip, B: Skip};Close, &{A: Skip, B: Skip, C:Skip};Wait) -> ()
f p = let (c,b) = p in
      fork (\_:() 1-> c |> select A |> close);
      match b with {
        A c -> wait c,
        B c -> wait c
      }

main : ()
main = f $ new @(+{A: Skip, B: Skip};Close) ()