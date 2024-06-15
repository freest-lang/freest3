type BoolServer = &{ And: ?Bool; ?Bool; !Bool; Skip
                   , Or : ?Bool; ?Bool; !Bool; Skip
                   , Not: ?Bool; !Bool; Skip
                   }
                   ; Wait
type BoolClient = dualof BoolServer

boolServer : BoolServer -> ()
boolServer c =
  match c with {
    And c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      c3 |> send (n1 && n2) |> wait,
    Or c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      c3 |> send (n1 || n2) |> wait,
    Not c1 ->
      let (n1, c2) = receive c1 in
      c2 |> send (not n1) |> wait
  }

client1 : BoolClient -> Bool
client1 w = w |> select Or
              |> send True
              |> send False
              |> receiveAndClose @Bool 


main : Bool
main =
  let (w,r) = new @BoolClient () in
  let x = fork @() (\_:()1-> boolServer r) in
  client1 w

-- remove skips from the end
-- Type check : environment checks only the linear part (filter)
