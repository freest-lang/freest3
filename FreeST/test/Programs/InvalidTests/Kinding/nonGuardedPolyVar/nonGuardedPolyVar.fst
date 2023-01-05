f : ∀ α: 1S . (rec b: 1S . α ; b) -> ()
f x = f @α x

main : ()
main = let (r, _) = new @Skip () in
       f  @Skip r
