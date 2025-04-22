type BoolServer = &{  And: Skip ; ?Bool ; ?Bool ; !Bool
                    , Or : Skip ; ?Bool ; ?Bool ; !Bool
                    , Not: Skip ; ?Bool ; !Bool
                  } ; Wait

boolServer :  BoolServer -> ()
boolServer c =
  match c with {
    And c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      send (n1 && n2) c3 
      |> wait,

    Or c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      send (n1 || n2) c3
      |> wait,

    Not c1 ->
      let (n1, c2) = receive c1 in
      send (not n1) c2
      |> wait
  }

main : ()
main = 
  newHcServer @BoolServer ("127.0.0.1", "8081") |>
  boolServer ;
  ()

-- remove skips from the end
-- Type check : environment checks only the linear part (filter)
