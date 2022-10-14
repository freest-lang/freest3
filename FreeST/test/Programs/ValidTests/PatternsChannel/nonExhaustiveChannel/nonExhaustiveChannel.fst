
type C : 1S = &{A: Skip, B: Skip}

f : C 1-> Int
f (A c) = 0

main : Int
main = 
  let (w,r) = new C in
  select A r;
  f w