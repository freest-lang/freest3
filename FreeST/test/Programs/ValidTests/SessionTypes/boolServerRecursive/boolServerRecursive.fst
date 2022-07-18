boolServer : (rec x: 1S. &{And: ?Bool;?Bool;!Bool;x, Or: ?Bool;?Bool;!Bool;x, Not: ?Bool;!Bool;x, Done: Skip}) -> ()
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

    Done c ->
      ()
  }

client1 : (rec x: 1S. +{And: !Bool;!Bool;?Bool;x, Or: !Bool;!Bool;?Bool;x, Not: !Bool;?Bool;x,Done: Skip}) -> Bool
client1 c =
  let c = select And c in
  let c = send True c in
  let c = send True c in
  let (x, c) = receive c in
  let c = select Not c in
  let c = send x c in
  let (y, c) = receive c in
  let c = select Done c in
  y

main : Bool
main =
  let (w, r) = new rec x: 1S. +{And: !Bool;!Bool;?Bool;x, Or: !Bool;!Bool;?Bool;x, Not: !Bool;?Bool;x, Done: Skip} in
  let x = fork @() (boolServer r) in
  client1 w
