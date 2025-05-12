-- Production S0
type S0 = (Skip; ?Int) ; ?Int

-- The server offers the choice composed by A
server : S0;Wait -> ()
server c =
    let (_, c1) = receive c in
    let (_, c2) = receive c1 in
    wait c2 ;
    ()

main : ()
main =
  newHcServer @(S0; Wait) ("127.0.0.1", "8081") |>
  server ; 
  ()