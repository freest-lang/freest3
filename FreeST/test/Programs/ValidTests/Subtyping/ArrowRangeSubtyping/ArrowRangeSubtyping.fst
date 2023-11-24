f : (() 1-> +{A: Skip, B: Skip};Close) -> ()
f g = g () |> select B |> close 


main : ()
main = let (c,b) = new @(+{A: Skip, B: Skip, C: Skip};Close) ()  in
       fork (\_:() 1-> f (\_:() 1-> c));
       match b with {
        A c -> wait c,
        B c -> wait c,
        C c -> wait c
       }