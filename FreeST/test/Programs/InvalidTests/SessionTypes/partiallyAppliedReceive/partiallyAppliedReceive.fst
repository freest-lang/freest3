f : ?Int 1-> (Int, Skip)
f = receive

main : (Int, Skip)
main =
  let (r, w) = new @!Int () in
  let _ = fork @Skip (\_:() 1-> send 5 r) in
  f w
