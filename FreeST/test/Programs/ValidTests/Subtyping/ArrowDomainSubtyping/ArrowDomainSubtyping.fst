f : (+{A: Skip, B: Skip} -> Skip) -> +{A: Skip, B: Skip} -> Skip
f g c = g c 


main : Skip
main = let (c,b) = new +{A: Skip, B: Skip} in
       f (\c:(+{A: Skip}) -> select A c) c;
       match b with {
        A c -> c, 
        B c -> c
       }