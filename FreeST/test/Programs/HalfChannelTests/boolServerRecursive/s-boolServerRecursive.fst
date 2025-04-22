type BoolServer = &{ And : ?Bool; ?Bool; !Bool; BoolServer
                   , Or  : ?Bool; ?Bool; !Bool; BoolServer
                   , Not : ?Bool; !Bool; BoolServer
                   , Done: Wait
                   }


boolServer : BoolServer -> ()
boolServer c =
  match c with {
    And c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      let c = send (n1 && n2) c in
      boolServer c,
    Or c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      let c = send (n1 || n2) c in
      boolServer c,
    Not c ->
      let (n, c) = receive c in
      (boolServer (send (not n) c)),
    Done c -> wait c
  }

main : ()
main =
  newHcServer @BoolServer ("127.0.0.1", "8081") |>
  boolServer ;
  ()