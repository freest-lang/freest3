boolServer ::  &{And: Skip;?Bool;?Bool;!Bool;Skip,
                Or: Skip;?Bool;?Bool;!Bool;Skip,
                Not: Skip;?Bool;!Bool;Skip} -> ()
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
      ()
      
start :: Bool
start = 
  s1


s1 :: Bool
s1 = 
  let c1 = startClient client1 in
  let c2 = startClient client2 in
  c1 || c2
  
client1 :: +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} -> Bool
client1 w =
  let w1 = select And w in
  let w2 = send True w1 in
  let r1 = send False w2 in
  let x, r2 = receive r1 in
  x

client2 :: +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} -> Bool
client2 w =
  let w1 = select Not w in
  let r1 = send True w1 in
  let x, r2 = receive r1 in
  x


startClient :: (+{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} -> Bool) -> Bool
startClient client =
  let w,r = new +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} in
  let x = fork (boolServer r) in
  client w

  
-- remove skips from the end
-- Type check : environment checks only the linear part (filter)

