type T = End

main : ()
main = let (r,w) = new @End () in fork (\_:()1-> close r); close w
