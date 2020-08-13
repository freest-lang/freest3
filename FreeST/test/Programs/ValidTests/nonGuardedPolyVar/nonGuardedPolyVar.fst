f : forall α:SL => (rec b:SL . Skip ; rec c:SL . α ; b) -> ()
f x = f[α] x

main : ()
main = let (r, _) = new Skip in
       fork (f[Skip] r)
