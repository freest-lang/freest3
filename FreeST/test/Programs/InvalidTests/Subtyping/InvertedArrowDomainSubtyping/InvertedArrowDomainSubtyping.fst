f : (+{A: Skip, B: Skip};End -> ()) -> +{A: Skip, B: Skip};End -> ()
f g c = g c 


main : ()
main = let (c,b) = new @(+{A: Skip, B: Skip};End) () in
       fork (\_:() 1-> f (\c:+{A: Skip, B: Skip, C: Skip};End -> c |> select C |> close) c);
       match b with {
        A c -> close c,
        B c -> close c
       }