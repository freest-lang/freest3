type MathServer = &{Negate: ?Int;!Int, Add: ?Int;?Int;!Int};End
type MathClient = dualof MathServer

mathServer : MathServer -> ()
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
  let (w,r) = new @MathClient () in
  fork @() (\_:()1-> mathServer r);
  w |> select Add 
    |> send 5 
    |> send 18
    |> receiveAndClose @Int
