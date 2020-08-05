f : forall α:SL => (rec b:SL . α; b) -> ()
f x = f[a] x

main : ()
main = let (r, _) = new Skip in
       fork (f[Skip] r)
