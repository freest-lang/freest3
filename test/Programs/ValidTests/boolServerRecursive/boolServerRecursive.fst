boolServer : (rec x:SU. &{And: ?Bool;?Bool;!Bool;x, Or: ?Bool;?Bool;!Bool;x, Not: ?Bool;!Bool;x, End: Skip}) -> ()
boolServer c =
  match c with {
    And c -> 
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      let c = send c (n1 && n2) in
      boolServer c,

    Or c -> 
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      let c = send c (n1 || n2) in 
      boolServer c,
    Not c -> 
      let (n, c) = receive c in
      -- let c = send c (not n) in
      -- boolServer c,
      (boolServer (send c (not n))),

    End c ->
      ()
  }

client1 : (rec x:SU. +{And: !Bool;!Bool;?Bool;x, Or: !Bool;!Bool;?Bool;x, Not: !Bool;?Bool;x,End: Skip}) -> Bool
client1 c =
  let c = select c And in
  let c = send c True in
  let c = send c True in
  let (x, c) = receive c in
  let c = select c Not in
  let c = send c x in
  let (y, c) = receive c in
  let c = select c End in
  y

main : Bool
main =
  let (w, r) = new rec x:SU. +{And: !Bool;!Bool;?Bool;x, Or: !Bool;!Bool;?Bool;x, Not: !Bool;?Bool;x, End: Skip} in
  let x = fork (boolServer r) in
  client1 w
