
type C = &{A: Skip, B: Skip}

f : C;Close 1-> Int
f (A c) = close c ; 0

main : Int
main = 
  let (w,r) = new @(C;Close) () in
  fork (\_:() 1-> select B r |> wait);
  f w
