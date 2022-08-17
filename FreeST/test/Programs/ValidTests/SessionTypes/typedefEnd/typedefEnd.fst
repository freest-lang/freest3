type T : 1S = End

main : ()
main = let (r,w) = new End in fork (close r); close w