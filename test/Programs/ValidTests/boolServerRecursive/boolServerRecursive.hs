boolServer :: (rec x . &{And: Skip;?Bool;?Bool;!Bool;Skip,
                Or: Skip;?Bool;?Bool;!Bool;Skip,
                Not: Skip;?Bool;!Bool;x,
                End: Skip}) -> ()
boolServer c =
  match c with
    And c1 -> 
      let n1, c2 = receive c1 in
      let n2, c3 = receive c2 in
      let x = send (n1 && n2) c3 in
      ()

    Or c1 -> 
      let n1, c2 = receive c1 in
      let n2, c3 = receive c2 in
      let x = send (n1 || n2) c3 in 
      ()

    Not c1 -> 
      let n1, c2 = receive c1 in
      let x = send (not n1) c2 in
      (boolServer x)

    End c1 -> ()
      
      

start :: Bool
start =
  startClient client1

client1 :: (rec x . +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;x,End: Skip}) -> Bool
client1 w =
  let w1 = select Not w in
  let r1 = send True w1 in
  let x, w2 = receive r1 in
  let w3 = select Not w2 in
  let r2 = send x w3 in
  let y, w4 = receive r2 in
  let w5 = select End w4 in
  y


startClient :: ((rec x . +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;x, End: Skip}) -> Bool) -> Bool
startClient client =
  let w,r = new (rec x . +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;x, End: Skip}) in
  let x = fork (boolServer r) in
  client w

  
-- remove skips from the end
