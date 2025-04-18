g : ?Char;a -> a
g c = let (_,c) = receive c in c

reader : T -> ()
reader c =
  let (i, c) = receive c in
  print @Int i;
  reader c

type T : 1S = ?Int;T;!Int

main : ()
main =
  newHcServer @(?Char;T) ("127.0.0.1", "8081") |>
  g @(T) |> reader

