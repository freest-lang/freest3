
type C = &{A: Skip, B: Skip};Wait

f : C 1-> Int
f (A c) = wait c; 0

main : Int
main = 
  let (w,r) = new @C () in
  r |> select B |> close;
  f w
