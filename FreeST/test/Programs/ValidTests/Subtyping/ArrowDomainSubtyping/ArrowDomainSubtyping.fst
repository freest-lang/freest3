f : (+{A: Skip, B: Skip};Close -> ()) -> +{A: Skip, B: Skip};Close -> ()
f g c = g c 


main : ()
main = let (c,b) = new @(+{A: Skip, B: Skip};Close) () in
       fork (\_:() 1-> f (\c:(+{A: Skip};Close) -> c |> select A |> close) c);
       match b with {
        A c -> wait c, 
        B c -> wait c 
       }