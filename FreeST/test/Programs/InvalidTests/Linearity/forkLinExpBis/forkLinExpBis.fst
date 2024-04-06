f : forall a: 1S . a -> (Int, a)
f x = (7, x)

main : Int
main =
  let (s, r) = new @(!Char;Close) () in
  let _ = fork @(Int, ?Char;Wait) (\_:() 1-> f  @(?Char;Wait) r) in
  s |> send 'a' |> close; 
  5
