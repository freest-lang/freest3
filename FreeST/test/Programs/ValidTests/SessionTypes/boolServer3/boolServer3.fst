type BoolServer : 1S = &{ And: Skip; ?Bool; ?Bool; !Bool; Skip
                        , Or : Skip; ?Bool; ?Bool; !Bool; Skip
                        , Not: Skip; ?Bool; !Bool; Skip
                        }
                        ; EndW
type BoolClient : 1S = dualof BoolServer

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

main : Bool
main = s1

s1 : Bool
s1 =
  let c1 = startClient client1 in
  let c2 = startClient client2 in
  c1 || c2

client1 : BoolClient -> Bool
client1 w = w |> select And
              |> send True
              |> send False
              |> receiveAndClose @Bool

client2 : BoolClient -> Bool
client2 w = w |> select Not
              |> send True
              |> receiveAndClose @Bool


startClient : (BoolClient -> Bool) -> Bool
startClient client =
  let (w,r) = new @BoolClient () in
  fork @() $ (\_:()1-> boolServer r);
  client w


-- remove skips from the end
-- Type check : environment checks only the linear part (filter)
