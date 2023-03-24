f : (+{A: Skip, B: Skip};End, &{A: Skip, B: Skip, C:Skip};End) -> ()
f p = let (c,b) = p in
      fork (\_:() 1-> c |> select A |> close);
      match b with {
        A c -> close c,
        B c -> close c
      }

main : ()
main = f $ new @(+{A: Skip, B: Skip};End) ()