f : (() 1-> +{A: Skip, B: Skip};End) -> ()
f g = g () |> select B |> close 


main : ()
main = let (c,b) = new @(+{A: Skip};End) () in
       fork (\_:() 1-> f (\_:() 1-> c));
       match b with {
        A c -> close c 
       }