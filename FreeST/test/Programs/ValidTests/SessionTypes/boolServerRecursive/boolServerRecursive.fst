type BoolServer : 1S = &{ And : ?Bool; ?Bool; !Bool; BoolServer
                        , Or  : ?Bool; ?Bool; !Bool; BoolServer
                        , Not : ?Bool; !Bool; BoolServer
                        , Done: End
                        }
type BoolClient : 1S = dualof BoolServer

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
      -- let c = send c (not n) in
      -- boolServer c,
      (boolServer (send (not n) c)),
    Done c -> close c
  }

client1 : BoolClient -> Bool
client1 c =
  let (x, c) = 
    select And c
    & send True
    & send True
    & receive in 
  let (y, c) = 
    select Not c
    & send x 
    & receive in
  select Done c & close ;
  y

main : Bool
main =
  let (w, r) = new BoolClient in
  let x = fork @() (boolServer r) in
  client1 w
