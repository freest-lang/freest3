type T : 1S = Wait

main : ()
main = let (r,w) = new @T () in fork (\_:()1-> wait r); close w
