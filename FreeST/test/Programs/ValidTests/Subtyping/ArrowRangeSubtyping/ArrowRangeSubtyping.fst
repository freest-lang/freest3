f : (() 1-> +{A: Skip, B: Skip}) -> Skip
f g = g () |> select B


main : Skip
main = let (c,b) = new +{A: Skip, B: Skip, C: Skip} in
       f (\_:() 1-> c);
       match b with {
        A c -> c,
        B c -> c,
        C c -> c
       }