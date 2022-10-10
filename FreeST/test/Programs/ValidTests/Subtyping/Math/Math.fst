type MathServer : 1S = &{Negate: ?Int;!Int, Add: ?Int;?Int;!Int};End
type MathClient : 1S = +{Negate: !Int;?Int                     };End

mathServer : MathServer-> ()
mathServer c =
  match c with {
    Negate c ->
      let (n, c) = receive c in
      send (-n) c
      |> close,
    Add c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      send (n1 + n2) c
      |> close
  }

main : Int
main =
  let (w,r) = new MathClient in
  let _ = fork (\_:()1-> mathServer r) in
  let (x, w) = select Negate w |> send 5 |> receive in
  close w;
  x
