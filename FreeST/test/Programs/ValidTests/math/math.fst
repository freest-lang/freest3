type MathServer = &{Neg: ?Int;!Int, Add: ?Int;?Int;!Int}

mathServer : MathServer -> Skip
mathServer c =
  match c with {
    Neg c ->
      let (n, c) = receive c in
      send (-n) c,
    Add c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      send (n1 + n2) c
  }

main : Int
main =
  let (r,w) = new MathServer in
  fork (mathServer r);
  let (x, _) = receive (send 5 (select Add w)) in
  -- let (x, _) = receive (send 5 (send 7 (select Add w))) in
  x
