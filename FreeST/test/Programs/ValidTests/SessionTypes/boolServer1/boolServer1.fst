type BoolServer = &{ And: ?Bool; ?Bool; !Bool; Skip
                        , Or : ?Bool; ?Bool; !Bool; Skip
                        , Not: ?Bool; !Bool; Skip
                        }
                        ; End
type BoolClient = dualof BoolServer

boolServer : BoolServer -> ()
boolServer c =
  match c with {
    And c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      send (n1 && n2) c3 
      |> close,
    Or c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      send (n1 || n2) c3
      |> close,
    Not c1 ->
      let (n1, c2) = receive c1 in
      send (not n1) c2
      |> close
  }

main : Bool
main =
  let (w,r) = new @BoolClient () in
  let x = fork @() (\_:()1-> boolServer r) in
  let ret = client1 w in
  ret



client1 : BoolClient -> Bool
client1 w = w |> select Or
              |> send True
              |> send False
              |> receiveAndClose @Bool 

-- remove skips from the end
-- Type check : environment checks only the linear part (filter)
