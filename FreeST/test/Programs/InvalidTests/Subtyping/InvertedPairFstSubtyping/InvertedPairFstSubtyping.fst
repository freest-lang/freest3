f : (&{A: Skip, B: Skip}, +{A: Skip, B: Skip}) -> ()
f p = let (b,c) = p in
      select A c;
      match b with {
        A c -> (),
        B c -> ()
      }

main : ()
main = f (new &{A: Skip, B: Skip, C:Skip})