f : !Char;a -> a
f c = let c = send 'a' c in c

writer : Int -> T -> ()
writer i c =
  let _ = writer (i + 1) (send i c)
  in ()

type T : 1S = !Int;T;?Int

main : ()
main =
  newHcClient1 @(!Char;T) ("127.0.0.1", "8081") |>
  f @T |> writer 0
