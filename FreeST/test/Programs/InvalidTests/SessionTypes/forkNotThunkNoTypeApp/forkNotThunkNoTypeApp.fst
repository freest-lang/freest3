main : Int
main =
  let (w, r) = new @(!Int;Close) () in
  fork (w |> send 5 |> close);
  receiveAndWait @Int r

