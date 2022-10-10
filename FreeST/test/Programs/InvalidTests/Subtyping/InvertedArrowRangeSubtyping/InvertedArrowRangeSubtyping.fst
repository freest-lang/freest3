f : (() -> +{A: Skip, B: Skip}) -> Skip
f g = g () |> select B


main : Skip
main = let (c,b) = new +{A: Skip} in
       f (\_:() -> c);
       match b with {
        A c -> c
       }