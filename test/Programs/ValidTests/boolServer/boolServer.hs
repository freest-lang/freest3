


boolServer :: &{And: ?Bool;?Bool;!Bool;Skip,
                Or: ?Bool;?Bool;!Bool;Skip,
                Not: ?Bool;!Bool;Skip} -> ()
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
  let w,r = new +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} in
  let x = fork (boolServer r) in
  client1 w


client1 :: +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} -> Bool
client1 w =
  let w1 = select And w in
  let w2 = send True w1 in
  let r1 = send False w2 in
  let x, r2 = receive r1 in
  x
  
  
a :: Int
a = 2

b :: Int
b = 2 + 2

c :: (Int,Bool)
c = (2, False)
