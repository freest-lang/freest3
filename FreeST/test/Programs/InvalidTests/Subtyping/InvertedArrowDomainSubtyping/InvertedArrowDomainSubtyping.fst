f : (+{A: Skip, B: Skip} -> Skip) -> +{A: Skip, B: Skip} -> Skip
f g c = g c 


main : Skip
main = let (c,b) = new +{A: Skip, B: Skip} in
       f (\c:+{A: Skip, B: Skip, C: Skip} -> select C c) c;
       match b with {
        A c -> c,
        B c -> c
       }