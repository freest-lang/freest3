-- TEST ERROR MESSAGES

type Value = Int
type Triple : 1T = (!Int, (Value, Value))
type Pair : 1T = (!Int, Value)

pairToValue : Pair -> Value
pairToValue p =
  let (x, y) = p in x+y

sendValue : Triple -> ()
sendValue t =
  let (c, pair) = t in
  let _ = send c (pairToValue pair) in
  ()

rcvValue : ?Int -> Value
rcvValue c = let (v, c) = receive c in v
  
 
main : Value
main =
  let (x, y) = new !Int in   
  let aTriple = (x, (2, 3)) in
  let _ = fork @() (\_:() 1-> sendValue aTriple) in
  let (x, _) = receive y in x     
